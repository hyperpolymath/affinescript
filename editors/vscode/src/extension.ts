// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// AffineScript VSCode Extension

import * as vscode from 'vscode';
import * as path from 'path';
import { exec } from 'child_process';
import { promisify } from 'util';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Executable
} from 'vscode-languageclient/node';

const execAsync = promisify(exec);

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  console.log('AffineScript extension activated');

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand('affinescript.check', checkCurrentFile),
    vscode.commands.registerCommand('affinescript.eval', evalCurrentFile),
    vscode.commands.registerCommand('affinescript.compile', compileCurrentFile),
    vscode.commands.registerCommand('affinescript.format', formatCurrentFile),
    vscode.commands.registerCommand('affinescript.restartLsp', restartLsp)
  );

  // Start LSP if enabled
  const config = vscode.workspace.getConfiguration('affinescript');
  if (config.get('lsp.enabled')) {
    startLsp(context);
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

async function startLsp(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration('affinescript');
  const serverPath = config.get<string>('lsp.serverPath') || 'affinescript-lsp';

  // Check if LSP server exists
  try {
    await execAsync(`which ${serverPath}`);
  } catch {
    vscode.window.showWarningMessage(
      `AffineScript LSP server not found at '${serverPath}'. Language features disabled.`
    );
    return;
  }

  const serverOptions: ServerOptions = {
    run: { command: serverPath } as Executable,
    debug: { command: serverPath } as Executable
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'affinescript' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.as')
    }
  };

  client = new LanguageClient(
    'affinescriptLsp',
    'AffineScript Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
  vscode.window.showInformationMessage('AffineScript LSP started');
}

async function restartLsp() {
  if (client) {
    await client.stop();
    vscode.window.showInformationMessage('AffineScript LSP stopped');
  }

  const context = (global as any).extensionContext;
  if (context) {
    await startLsp(context);
  }
}

async function checkCurrentFile() {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'affinescript') {
    vscode.window.showErrorMessage('No AffineScript file open');
    return;
  }

  const filePath = editor.document.uri.fsPath;
  const terminal = vscode.window.createTerminal('AffineScript Check');
  terminal.show();
  terminal.sendText(`affinescript check "${filePath}"`);
}

async function evalCurrentFile() {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'affinescript') {
    vscode.window.showErrorMessage('No AffineScript file open');
    return;
  }

  const filePath = editor.document.uri.fsPath;
  const terminal = vscode.window.createTerminal('AffineScript Eval');
  terminal.show();
  terminal.sendText(`affinescript eval "${filePath}"`);
}

async function compileCurrentFile() {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'affinescript') {
    vscode.window.showErrorMessage('No AffineScript file open');
    return;
  }

  const filePath = editor.document.uri.fsPath;
  const outputPath = filePath.replace(/\.as$/, '.wasm');

  const terminal = vscode.window.createTerminal('AffineScript Compile');
  terminal.show();
  terminal.sendText(`affinescript compile "${filePath}" -o "${outputPath}"`);
}

async function formatCurrentFile() {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'affinescript') {
    vscode.window.showErrorMessage('No AffineScript file open');
    return;
  }

  const filePath = editor.document.uri.fsPath;

  try {
    const { stdout } = await execAsync(`affinescript fmt "${filePath}"`);
    vscode.window.showInformationMessage('File formatted successfully');
  } catch (error: any) {
    vscode.window.showErrorMessage(`Formatting failed: ${error.message}`);
  }
}
