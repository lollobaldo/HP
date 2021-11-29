import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as cp from 'child_process';

import { InteractiveProcessHandle } from './repljs';

const showProgress = () => {
  let ret: (value: void | PromiseLike<void>) => void;
  const promise = new Promise<void>(resolve => {
      ret = resolve;
  });
  vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: "Working on it! May take some time...",
    cancellable: false
  }, (progress, token) => promise);
  return { end: ret };
};

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

const invalidateCache = async (filePath: fs.PathLike) => {
  let data = await readFile(filePath);
  console.log(data[0]);
  if (data[0] === '\n') data = data.substring(1); else data = '\n' + data;
  console.log(data[0])
  try {
    return fs.promises.writeFile(filePath, data, 'utf8');
  } catch (err) {
    console.error('Error occured while writing back file!', err);
  }
};

export function activate(context: vscode.ExtensionContext) {
  let _ghciInstance: InteractiveProcessHandle;
  let _activeCwd = '';
  
  const getGhci = async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      throw "No active editor to use as cwd";
    }

    const filename = editor.document.fileName;
    let dir = path.parse(filename).dir;
    // Fix for Windows uppercase requirement for drive letters
    if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());
    const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "\/");

    console.log(_ghciInstance, cwd, _activeCwd);
    if (!_ghciInstance || cwd !== _activeCwd) {
      const cmd = `cabal repl Main --ghc-options=-i${dir}`.replace(/\\/g, "\/");
      console.log(cmd, cwd);
      _ghciInstance = new InteractiveProcessHandle(cmd, [], { cwd });
      _activeCwd = cwd;
    }
    return _ghciInstance;
  };

  const generateHtml = async (ghciInstancePromise: Promise<InteractiveProcessHandle>, identifier: string) => {
    const ghciInstance = await ghciInstancePromise;
    await ghciInstance.call(':l Main');
    await ghciInstance.call(`graph File.${identifier}`);
    const data = await readFile(path.join(context.extensionPath, 'interactive-map', 'out', 'out1.html'));
    return data;
  };

  context.subscriptions.push(
    vscode.commands.registerCommand('catCoding.waffle', async () => {
      const ghciInstance = await getGhci();
      const d = await ghciInstance.call(':r');
      console.log(d);
      return;
    }),
    vscode.commands.registerCommand('catCoding.start', async () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      const ghciInstancePromise = getGhci();
      const editor = vscode.window.activeTextEditor;
      const document = editor.document;
      
      const progressNotification = showProgress();

      const filename = editor.document.fileName;
      let { name, dir } = path.parse(filename);
      if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());

      const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'templates', 'Main.template.hs');
      const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
      
      const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
      const highlight = editor.document.getText(wordRange);
      await replaceInFile(tempDispPath, injeDispPath, [
        ["###REPLACE WITH NAME OF MODULE###", name],
        // ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight]
      ]);

      console.log(highlight);

      const line = editor.selection.active.line;
      const inset = vscode.window.createWebviewTextEditorInset(
          vscode.window.activeTextEditor, line-1, 12,
          { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
      );
      inset.webview.onDidReceiveMessage(
        async message => {
          console.log(message);
          const progress = showProgress();
          // vscode.window.showErrorMessage(message.id);
          const { key, value, isRemove } = message;
          
          const exp = isRemove ? 'Nothing' : `Just ${value}`;
          console.log(isRemove, exp);

          const ghciInstance = await ghciInstancePromise;
          await ghciInstance.call(':l Main');
          const result = await ghciInstance.call(`edit (File.${highlight}) (${key}) (${exp})`);
          console.log("RESULT:");
          console.log(result);
          var startposition = new vscode.Position(line,0);
          var endingposition = new vscode.Position(line+1,0);
          var range = new vscode.Range(startposition,endingposition);
          editor.edit(editBuilder => {
            editBuilder.replace(range, `${highlight} = ${result}\n`);
          });
          await document.save();
          inset.webview.html = await generateHtml(ghciInstancePromise, highlight);
          progress.end()
          return;
        },
        undefined,
        context.subscriptions
      );
      inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
      inset.webview.html = await generateHtml(ghciInstancePromise, highlight);
      progressNotification.end()
    })
  );
}
