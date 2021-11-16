import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as cp from 'child_process';

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
  context.subscriptions.push(
    vscode.commands.registerCommand('catCoding.start', async () => {
      if (!vscode.window.activeTextEditor) {
        return;
      }
      const editor = vscode.window.activeTextEditor;
      
      const filename = editor.document.fileName;
      let { name, dir } = path.parse(filename);
      if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());
      console.log(filename, dir, name);

      const tempSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci.template');
      const injeSettingPath = path.join(context.extensionPath, 'interactive-map', '.ghci');
      await replaceInFile(tempSettingPath, injeSettingPath, [["###REPLACE WITH DIRECTORY OF PROJECT###", dir]]);

      const tempMainPath = path.join(context.extensionPath, 'interactive-map', 'MainDisplay.hs.template');
      const injeMainPath = path.join(context.extensionPath, 'interactive-map', 'MainDisplay.hs');
      
      const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
      const highlight = editor.document.getText(wordRange);
      await replaceInFile(tempMainPath, injeMainPath, [
        ["###REPLACE WITH NAME OF MODULE###", name],
        ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight]
      ]);
      console.log(highlight);

      const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "\/");
      console.log(cwd);
      const cmd = `cabal run MainDisplay --ghc-options=-i${dir}`.replace(/\\/g, "\/");
      console.log(cmd);
      try {
        console.log(cp.execSync(cmd, { cwd }).toString());
      }
      catch(err) {
        console.log(err);
      }
      const data = await readFile(path.join(context.extensionPath, 'interactive-map', 'out1.html'));

      const line = editor.selection.active.line;
      const inset = vscode.window.createWebviewTextEditorInset(
          vscode.window.activeTextEditor, line-1, 12,
          { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
      );
      inset.webview.onDidReceiveMessage(
        async message => {
          console.log(message);
          vscode.window.showErrorMessage(message.id);
          const { key, value, isRemove } = message;
          
          const exp = isRemove ? 'Nothing' : `Just ${value}`;
          console.log(isRemove, exp);

          const tempMainPath = path.join(context.extensionPath, 'interactive-map', 'MainEdit.hs.template');
          const injeMainPath = path.join(context.extensionPath, 'interactive-map', 'MainEdit.hs');

          await replaceInFile(tempMainPath, injeMainPath, [
            ["###REPLACE WITH NAME OF MODULE###", name],
            ["###REPLACE WITH IDENTIFIER OF EXPRESSION###", highlight],
            ["###REPLACE WITH KEY###", key],
            ["###REPLACE WITH VALUE###", exp]
          ]);
          const cmd = `cabal run MainEdit --ghc-options=-i${dir}`.replace(/\\/g, "\/");
          console.log(cmd);
          let result = "";
          try {
            result = cp.execSync(cmd, { cwd }).toString();
          }
          catch(err) {
            console.log(err);
            return;
          }
          console.log(result);
          const mid = result.split('\n');
          const newValue = mid[mid.length - 2];
          var startposition = new vscode.Position(line,0);
          var endingposition = new vscode.Position(line+1,0);
          var range = new vscode.Range(startposition,endingposition);
          editor.edit(editBuilder => {
            editBuilder.replace(range, `${highlight} = ${newValue}\n`);
          });
          console.log(`${highlight} = ${newValue}`);
          console.log(range);
          return;
        },
        undefined,
        context.subscriptions
      );
      inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
      inset.webview.html = data;
    })
  );
}
