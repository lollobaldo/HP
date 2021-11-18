"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.InteractiveProcessHandle = void 0;
const child_process = require('child_process');
class InteractiveProcessHandle {
    constructor(call, args, options) {
        this.replPrompt = "###DONE###";
        this.latestOutput = "";
        this.outputLog = "";
        this.process = child_process.spawn(call, args, { shell: true, ...options });
        this.process.stdout.setEncoding('utf8');
        this.process.stderr.setEncoding('utf8');
    }
    processToPromise(process) {
        return new Promise((resolve, reject) => {
            //   console.log("+++ creating promise");
            process.stdout.removeAllListeners();
            process.stderr.removeAllListeners();
            process.stdin.removeAllListeners();
            let lastString = '';
            process.stdout.on("data", (data) => {
                data = data.toString().trim();
                this.update(data);
                lastString += data;
                if (lastString.endsWith(this.replPrompt)) {
                    console.log('done', lastString);
                    resolve(lastString.replace(this.replPrompt, ''));
                }
            });
            process.stderr.on("data", (data) => {
                data = data.toString().trim();
                this.update(data);
                lastString += data;
                if (lastString.endsWith(this.replPrompt)) {
                    console.log('done', lastString);
                    resolve(lastString.replace(this.replPrompt, ''));
                }
            });
            process.stdin.on("error", () => {
                console.log("Failure in stdin! ... error");
                reject();
            });
            process.stdin.on("close", () => {
                console.log("Failure in stdin! ... close");
                reject();
            });
            process.stdin.on("end", () => {
                console.log("Failure in stdin! ... end");
                reject();
            });
            process.stdin.on("disconnect", () => {
                console.log("Failure in stdin! ... disconnect");
                reject();
            });
            process.stdout.on("error", () => {
                console.log("Failure in stdout! ... error");
                reject();
            });
            process.stdout.on("close", () => {
                console.log("Failure in stdout! ... close");
                reject();
            });
            process.stdout.on("end", () => {
                console.log("Failure in stdout! ... end");
                reject();
            });
            process.stderr.on("error", () => {
                console.log("Failure in stderr! ... error");
                reject();
            });
            process.stderr.on("close", () => {
                console.log("Failure in stderr! ... close");
                reject();
            });
            process.stderr.on("end", () => {
                console.log("Failure in stderr! ... end");
                reject();
            });
            //   console.log("+++ done creating promise");
        });
    }
    update(data) {
        this.latestOutput = data;
        this.outputLog += data + "\n";
        // console.log(`logging from update: "${data}"`);
    }
    call(command) {
        console.log(`called: "${command.trim()}"`);
        let promise = this.processToPromise(this.process);
        this.process.stdin.write(command + '\n');
        return promise;
    }
}
exports.InteractiveProcessHandle = InteractiveProcessHandle;
;
// var interactive_python = new InteractiveProcessHandle('python', ['-i']);
// async () => {
//   await interactive_python.call('');
//   await interactive_python.call('x = 20');
//   let x = await clang_build.call('print(x)\n');
//   console.log('x = ', x);
// }
//# sourceMappingURL=repljs.js.map