#![forbid(unsafe_code)]
// SPDX-License-Identifier: PMPL-1.0-or-later
//! Debug Adapter Protocol (DAP) implementation for Affinescript
//!
//! This is a minimal DAP adapter for Affinescript.

use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

#[derive(Debug, Serialize, Deserialize)]
struct DapRequest {
    seq: i64,
    r#type: String,
    command: String,
    arguments: Option<serde_json::Value>,
}

#[derive(Debug, Serialize)]
struct DapResponse {
    seq: i64,
    r#type: String,
    request_seq: i64,
    command: String,
    success: bool,
    message: Option<String>,
    body: Option<serde_json::Value>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let listener = TcpListener::bind("127.0.0.1:4714")?;
    println!("Affinescript DAP server listening on 127.0.0.1:4714");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                std::thread::spawn(|| {
                    if let Err(e) = handle_client(stream) {
                        eprintln!("Error handling client: {}", e);
                    }
                });
            }
            Err(e) => {
                eprintln!("Error accepting connection: {}", e);
            }
        }
    }

    Ok(())
}

fn handle_client(stream: TcpStream) -> Result<(), Box<dyn std::error::Error>> {
    let reader = BufReader::new(stream.try_clone()?);
    let mut writer = stream.try_clone()?;

    for line in reader.lines() {
        let line = line?;
        let request: DapRequest = serde_json::from_str(&line)?;
        let response = match request.command.as_str() {
            "initialize" => {
                serde_json::to_string(&DapResponse {
                    seq: 1,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "initialize".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({
                        "supportsConfigurationDoneRequest": true,
                        "supportsFunctionBreakpoints": true,
                        "supportsConditionalBreakpoints": true,
                        "supportsEvaluateForHovers": true,
                        "exceptionBreakpointFilters": [],
                    })),
                })?
            }
            "launch" => {
                serde_json::to_string(&DapResponse {
                    seq: 2,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "launch".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"success": true})),
                })?
            }
            "setBreakpoints" => {
                serde_json::to_string(&DapResponse {
                    seq: 3,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "setBreakpoints".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"breakpoints": []})),
                })?
            }
            "threads" => {
                serde_json::to_string(&DapResponse {
                    seq: 4,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "threads".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"threads": [{"id": 1, "name": "main"}]}));
                })?
            }
            "stackTrace" => {
                serde_json::to_string(&DapResponse {
                    seq: 5,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "stackTrace".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"stackFrames": []})),
                })?
            }
            "scopes" => {
                serde_json::to_string(&DapResponse {
                    seq: 6,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "scopes".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"scopes": [{"name": "Locals", "variablesReference": 1, "expensive": false}]}));
                })?
            }
            "variables" => {
                serde_json::to_string(&DapResponse {
                    seq: 7,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "variables".to_string(),
                    success: true,
                    message: None,
                    body: Some(serde_json::json!({"variables": []})),
                })?
            }
            "disconnect" => {
                serde_json::to_string(&DapResponse {
                    seq: 8,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: "disconnect".to_string(),
                    success: true,
                    message: None,
                    body: None,
                })?
            }
            _ => {
                serde_json::to_string(&DapResponse {
                    seq: 0,
                    r#type: "response".to_string(),
                    request_seq: request.seq,
                    command: request.command,
                    success: false,
                    message: Some("Unknown command".to_string()),
                    body: None,
                })?
            }
        };

        writeln!(writer, "{}", response)?;
    }

    Ok(())
}
