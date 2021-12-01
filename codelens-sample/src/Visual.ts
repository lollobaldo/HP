import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

import { InteractiveProcessHandle } from './repljs';
import { showProgress } from './utils';

type Repl = InteractiveProcessHandle;

interface Message {
  refresh?: boolean,
  key: string,
  value: string,
  isRemove: boolean,
}

export class Visual {
	ghciPromise: Promise<Repl>;
	identifier: string;
	line: number;
	inset: vscode.WebviewEditorInset;
	
	constructor(context: vscode.ExtensionContext, ghciPromise: Promise<Repl>, identifier: string, line: number) {
    if (!vscode.window.activeTextEditor) throw "No editor is active.";

	  this.ghciPromise = ghciPromise;
	  this.identifier = identifier;
	  this.line = line;
  
	  this.inset = vscode.window.createWebviewTextEditorInset(
		vscode.window.activeTextEditor, line-1, 12,
		{ localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
	  );
	  this.inset.webview.html = loadingPage;
	  this.inset.webview.onDidReceiveMessage(
        async (message: Message) => {
          console.log(message);
          if (!vscode.window.activeTextEditor) {
            throw "No editor is active.";
          }
          const progressNotification = showProgress();
          
          if (message.refresh) {
            await this.refreshHtml();
            progressNotification.end();
            return;
          };
          await this.crudAction(message);
          progressNotification.end();
          return;
        },
        undefined,
        context.subscriptions
      );
      this.inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
	}

  static async newVisual(context: vscode.ExtensionContext, ghciPromise: Promise<Repl>, identifier: string, line: number) {
    const a = new Visual(context, ghciPromise, identifier, line);
    await a.refreshHtml();
    return a;
  }
   
	async refreshHtml() {
    const ghciInstance = await this.ghciPromise;
    const load = await ghciInstance.call(':l Main');
    console.log("load:", load);
    const response = await ghciInstance.call(`graph File.${this.identifier}`);
    try{
      const parsed = JSON.parse(response);
      console.debug(parsed);
      this.inset.webview.html = parsed.html;
    }catch(e){
      console.error(e);
    };
	}

  async crudAction(message: Message) {
    if (!vscode.window.activeTextEditor) throw "No editor is active.";

    const { key, value, isRemove } = message;
          
    const exp = isRemove ? 'Nothing' : `Just ${value}`;
    console.log(isRemove, exp);

    const ghciInstance = await this.ghciPromise;
    await ghciInstance.call(':l Main');
    const response = await ghciInstance.call(`edit (File.${this.identifier}) (${key}) (${exp})`);
    const parsed = JSON.parse(response);
    console.debug(parsed);
    var startposition = new vscode.Position(this.line,0);
    var endingposition = new vscode.Position(this.line+1,0);
    var range = new vscode.Range(startposition,endingposition);
    vscode.window.activeTextEditor.edit(editBuilder => {
      editBuilder.replace(range, `${this.identifier} = ${parsed.code}\n`);
    });
    await vscode.window.activeTextEditor.document.save();

    this.refreshHtml();
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