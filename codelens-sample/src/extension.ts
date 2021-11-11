import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as cp from 'child_process';);

const readFile = async (filePath: fs.PathLike): Promise<string> => {
  try {
    return fs.promises.readFile(filePath, 'utf8');
  } catch (err) {
    console.error('Error occured while reading file!', err);
  }
  return "";
};

const replaceInFile = async (templatePath: fs.PathLike, filePath: fs.PathLike, reps: [RegExp | string, string][]) => {
  let data = await readFile(templatePath);
  for (const [s1, s2] of reps) {
    data = data.replace(s1, s2);
  }  
  console.log(data);
  try {
    return fs.promises.writeFile(filePath, data, 'utf8');
  } catch (err) {
    console.error('Error occured while writing back file!', err);
  }
};

export function activate(context: vscode.ExtensionContext) {
  // vscode.commands.registerCommand('myExtension.sayHello', () => {
  //   if (!vscode.window.activeTextEditor) {
  //       return;
  //   }

  //   const inset = vscode.window.createWebviewTextEditorInset(vscode.window.activeTextEditor, 2, 10);
  //   inset.onDidDispose(() => {
  //       console.log('WEBVIEW disposed...');
  //   });
  //   fs.readFile(path.join(context.extensionPath,'out1.html'),(err,data) => {
  //     if(err) { console.error(err); }
  //     console.log(data.toString());
  //     inset.webview.html = data.toString();
  //   });
  //   // inset.webview.html = `<head><meta></head><body><img src="https://imgs.xkcd.com/comics/plutonium.png"/><body>`;
  // });
  context.subscriptions.push(
    vscode.commands.registerCommand('catCoding.start', async () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      const editor = vscode.window.activeTextEditor;
      
      const filename = editor.document.fileName;
      const { name, dir } = path.parse(filename);
      console.log(filename, dir, name);

      const tempSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci.template');
      const injeSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci');
      await replaceInFile(tempSettingPath, injeSettingPath, [["###REPLACE WITH DIRECTORY OF PROJECT###", dir]]);

      const tempMainPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs.template');
      const injeMainPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
      
      const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
      const highlight = editor.document.getText(wordRange);
      await replaceInFile(tempMainPath, injeMainPath, [
        ["###REPLACE WITH NAME OF MODULE###", name],
        ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight]
      ]);
      console.log(highlight);

      cp.execSync('cabal run --ghc-options=-i/afs/inf.ed.ac.uk/user/s18/s1853050/UNI2/HP/demo', { cwd: path.join(context.extensionPath, 'interactive-map') });

      const data = await readFile(path.join(context.extensionPath, 'interactive-map', 'out1.html'));

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
      panel.webview.html = data.toString();
      // panel.webview.html = data;
      // Handle messages from the webview
      panel.webview.onDidReceiveMessage(
        message => {
          vscode.window.showErrorMessage(message.id);
          return;
        },
        undefined,
        context.subscriptions
      );

      // const line = editor.selection.active.line;
      // // const inset = vscode.window.createWebviewTextEditorInset(editor, line, 10);
      // const inset = vscode.window.createWebviewTextEditorInset(
      //     vscode.window.activeTextEditor, line-1, 30,
      //     { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
      //     );
      // inset.webview.onDidReceiveMessage(
      //   message => {
      //     vscode.window.showErrorMessage(message.id);
      //     return;
      //   },
      //   undefined,
      //   context.subscriptions
      // );
      // inset.onDidDispose(() => {
      //   console.log('WEBVIEW disposed...:(');
      // });
      // inset.webview.html = getWebviewContent();

      // await (ms => new Promise(resolve => setTimeout(resolve, ms)))(1000);

      // inset.webview.html = getWebviewContent();
      // console.log(inset);

      // const rootUrl = vscode.Uri.file(context.extensionPath);
      // const inset = vscode.window.createWebviewTextEditorInset(
      //   vscode.window.activeTextEditor, 5, 30,
      //   { localResourceRoots: [ rootUrl ], enableScripts: true, }
      //   );
      // inset.onDidDispose(() => {
      //     console.log('WEBVIEW disposed...');
      // });
      // inset.webview.html = getWebviewContent();
      // console.log("Hey");
    })
  );
}

// function getWebviewContent() {
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
// }

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

function getWebviewContent() {
    return `<!DOCTYPE html>
  <html lang="en">
  <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Cat Coding</title>
  </head>
  <body>
      <img src="https://media.giphy.com/media/JIX9t2j0ZTN9S/giphy.gif" width="300" />
  </body>
  </html>`;
  }
  
