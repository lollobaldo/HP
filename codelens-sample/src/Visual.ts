import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

import { InteractiveProcessHandle } from './repljs';

type Repl = InteractiveProcessHandle;

class Visual {
	ghciPromise: Promise<Repl>;
	identifier: string;
	span: number;
	inset: vscode.WebviewEditorInset;
	
	constructor(context: vscode.ExtensionContext, ghciPromise: Promise<Repl>, identifier: string, span: number) {
	  if (!vscode.window.activeTextEditor) {
		  throw "No editor is active.";
	  }
	  this.ghciPromise = ghciPromise;
	  this.identifier = identifier;
	  this.span = span;
  
	  const line = vscode.window.activeTextEditor.selection.active.line;
	  this.inset = vscode.window.createWebviewTextEditorInset(
		vscode.window.activeTextEditor, line-1, 12,
		{ localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
	  );
	  this.inset.webview.html = loadingPage;
	  this.inset.webview.onDidReceiveMessage(
        async message => {
          console.log(message);
          if (!vscode.window.activeTextEditor) {
            throw "No editor is active.";
          }
          const progressNotification = showProgress();
          
          if (message.refresh) {
            this.inset.webview.html = (await generateHtml(this.ghciPromise, identifier)).html;
            progressNotification.end();
            return;
          };

          const { key, value, isRemove } = message;
          
          const exp = isRemove ? 'Nothing' : `Just ${value}`;
          console.log(isRemove, exp);

          const ghciInstance = await ghciPromise;
          await ghciInstance.call(':l Main');
          const result = JSON.parse(await ghciInstance.call(`edit (File.${identifier}) (${key}) (${exp})`));
          console.log("RESULT:");
          console.log(result);
          var startposition = new vscode.Position(line,0);
          var endingposition = new vscode.Position(line+1,0);
          var range = new vscode.Range(startposition,endingposition);
          vscode.window.activeTextEditor.edit(editBuilder => {
            editBuilder.replace(range, `${identifier} = ${result.code}\n`);
          });
          await vscode.window.activeTextEditor.document.save();

          this.inset.webview.html = (await generateHtml(ghciInstance, identifier)).html;
          progressNotification.end();
          return;
        },
        undefined,
        context.subscriptions
      );
      this.inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
      const response = await generateHtml(ghciInstancePromise, identifier);
      console.log(response.info);
      this.inset.webview.html = response.html;
	}
   
	refreshHtml() {
	  return null;
	}
}
  

const loadingPage = '\
  <html><head>\
  <style>\
  .loader {\
	border: 16px solid #f3f3f3; /* Light grey */\
	border-top: 16px solid #3498db; /* Blue */\
	border-radius: 50%;\
	width: 120px;\
	height: 120px;\
	animation: spin 2s linear infinite;\
  }\
  \
  @keyframes spin {\
	0% { transform: rotate(0deg); }\
	100% { transform: rotate(360deg); }\
  }\
  </style>\
  <body><div class="loader"></div>\
';