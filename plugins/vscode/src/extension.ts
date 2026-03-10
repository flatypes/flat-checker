import path = require('path');
import net = require('net');
import * as vscode from 'vscode';
import { ServerOptions, LanguageClientOptions, LanguageClient } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
	console.log('FLAT-Checker extension is now is now active!');
  const serverOptions: ServerOptions =  () => {
    let socket = net.connect({ port: 5001, host: 'localhost' });
    return Promise.resolve({
      reader: socket,
      writer: socket
    });
  }

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'python' }]
  };
  client = new LanguageClient('flat-checker', 'FLAT-Checker Language Server', serverOptions, clientOptions);
  await client.start();
}

export async function deactivate() {
  await client?.dispose();
  console.log('FLAT-Checker extension is now deactivated.');
  client = undefined;
}
