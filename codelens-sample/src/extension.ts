// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { ExtensionContext, languages, commands, Disposable, workspace, window } from 'vscode';
import { CodelensProvider } from './CodelensProvider';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

let disposables: Disposable[] = [];

export function activate(context: ExtensionContext) {
    const codelensProvider = new CodelensProvider();

    languages.registerCodeLensProvider("*", codelensProvider);

    commands.registerCommand("codelens-sample.enableCodeLens", () => {
        workspace.getConfiguration("codelens-sample").update("enableCodeLens", true, true);
    });

    commands.registerCommand("codelens-sample.disableCodeLens", () => {
        workspace.getConfiguration("codelens-sample").update("enableCodeLens", false, true);
    });

    commands.registerCommand("codelens-sample.codelensAction", (args: any) => {
        console.log(args);
        window.showInformationMessage(`CodeLens action clicked with args=${args}`);
    });

    commands.registerCommand('extension.sayHello', async (args, brgs, crgs) => {
        if (!window.activeTextEditor) {
            console.log('quit');
            return;
        }
        console.log('generating');

        const inset = window.createWebviewTextEditorInset(window.activeTextEditor, 2, 10);
        inset.onDidDispose(() => {
            console.log('WEBVIEW disposed...');
        });
        inset.webview.html = `
        <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAAAUCAYAAAB7wJiVAAAAOUlEQVR42u3RQQ0AAAgEIK9/AcO6aQ0fUIFs1xRvRIgQhAhBiBCECEGIECFCECIEIUIQIgQhQvjiAOZxM5FSZq20AAAAAElFTkSuQmCC">
        `;
        console.log('set');
        console.log(inset.webview);
    });

}

// this method is called when your extension is deactivated
export function deactivate() {
    if (disposables) {
        disposables.forEach(item => item.dispose());
    }
    disposables = [];
}
