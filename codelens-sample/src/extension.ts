import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as cp from 'child_process';
import { privateEncrypt } from 'crypto';

export function activate(context: vscode.ExtensionContext) {
  vscode.commands.registerCommand('myExtension.sayHello', () => {
    if (!vscode.window.activeTextEditor) {
        return;
    }

    const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, 2, 10);
    inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...');
    });
    fs.readFile(path.join(context.extensionPath,'out1.html'),(err,data) => {
      if(err) { console.error(err); }
      console.log(data.toString());
      inset.webview.html = data.toString();
    });
    // inset.webview.html = `<head><meta></head><body><img src="https://imgs.xkcd.com/comics/plutonium.png"/><body>`;
  });
  context.subscriptions.push(
    vscode.commands.registerCommand('catCoding.start', () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      // Create and show panel
      const panel = vscode.window.createWebviewPanel(
        'catCoding',
        'Cat Coding',
        vscode.ViewColumn.One,
        {
          // Enable scripts in the webview
          enableScripts: true
        }
      );

      const filename = vscode.window.activeTextEditor.document.fileName;
      const folder = path.dirname(filename);
      console.log(folder);

      const runnerPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
      fs.readFile(runnerPath, 'utf8', function (err,data) {
        if (err) {
          return console.log(err);
        }
        const result = data.replace(/string to be replaced/g, 'replacement');
        fs.writeFile(someFile, result, 'utf8', function (err) {
           if (err) return console.log(err);
        });
      });
      

      const ghci = cp.spawn('cabal', ['repl', 'main']);
      // Event Standard Out.
      ghci.stdout.on('data', (data) => {
        console.log(data.toString('utf8'));
      });

      ghci.stdin.write('main\n');
      console.log(ghci.stdout);
      // And set its HTML content
      fs.readFile(path.join(context.extensionPath, 'interactive-map', 'out1.html'),(err,data) => {
        if(err) { console.error(err); }
        // console.log(data.toString());
        panel.webview.html = data.toString();
      });
      // panel.webview.html = getWebviewContent();
      // Handle messages from the webview
      panel.webview.onDidReceiveMessage(
        message => {
          vscode.window.showErrorMessage(message.id);
          return;
        },
        undefined,
        context.subscriptions
      );
    })
  );
}

function getWebviewContent() {
  // return(fs.readFile('out1.html',(err,data) => {
  //   if(err) { console.error(err); }
  //     resultPanel.webview.html = data;
  // }));
//   return `<!DOCTYPE html>
// <html lang="en">
// <head>
//     <meta charset="UTF-8">
//     <meta name="viewport" content="width=device-width, initial-scale=1.0">
//     <title>Cat Coding</title>
// </head>
// <body>
//     <img src="https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif" width="300" />
// </body>
// </html>`;
}

// // The module 'vscode' contains the VS Code extensibility API
// // Import the module and reference it with the alias vscode in your code below
// // import { ExtensionContext, commands, window } from 'vscode';
// import * as vscode from 'vscode';

// // this method is called when your extension is activated
// // your extension is activated the very first time the command is executed

// export function activate(context: vscode.ExtensionContext) {
//   vscode.commands.registerCommand('myExtension.sayHello', () => {
//     if (!vscode.window.activeTextEditor) {
//         return;
//     }

//     const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, 2, 10);
//     inset.onDidDispose(() => {
//         console.log('WEBVIEW disposed...');
//     });
//     inset.webview.html = `<head><meta></head><body><img src="https://imgs.xkcd.com/comics/plutonium.png"/><body>`;
//   });
// }

// function getWebviewContent() {
//     return `<!DOCTYPE html>
//   <html lang="en">
//   <head>
//       <meta charset="UTF-8">
//       <meta name="viewport" content="width=device-width, initial-scale=1.0">
//       <title>Cat Coding</title>
//   </head>
//   <body>
//       <img src="https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif" width="300" />
//   </body>
//   </html>`;
//   }
  
