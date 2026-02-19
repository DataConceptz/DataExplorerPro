# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.0.x   | :white_check_mark: |
| < 1.0   | :x:                |

## Reporting a Vulnerability

We take the security of DataExplorerPro seriously. If you discover a security vulnerability, please follow these steps:

### How to Report

1. **Do NOT open a public issue** on GitHub
2. Email your findings to polcollino@gmail.com3. Include the following information:
   - Type of vulnerability
   - Full path of source file(s) related to the vulnerability
   - Step-by-step instructions to reproduce
   - Proof-of-concept or exploit code (if possible)
   - Impact assessment

### What to Expect

- **Acknowledgment**: You will receive an acknowledgment within 48 hours
- **Assessment**: We will assess the vulnerability and determine its severity
- **Fix**: We will work on a fix and release it as soon as possible
- **Disclosure**: We will coordinate with you on responsible disclosure

### Security Best Practices

When using DataExplorerPro, please follow these security guidelines:

#### Ollama Configuration

1. **Local Only**: DataExplorerPro connects to Ollama at `http://localhost:11434` by default
2. **No Data Transmission**: Your data is processed locally and never sent to external servers
3. **Model Security**: Only use trusted Ollama models from official sources

#### Data Handling

1. **File Uploads**: Uploaded files are processed in-memory when possible
2. **Temporary Files**: Clean up temporary files after analysis
3. **Sensitive Data**: Be cautious when analyzing sensitive or personal data

#### Network Security

1. **Default Port**: The app runs on a random available port by default
2. **Local Access**: The app is only accessible from your local machine
3. **Remote Access**: If you need remote access, use proper authentication

### Known Security Considerations

#### R Code Execution

DataExplorerPro generates and executes R code based on natural language queries. This is by design but has security implications:

- Only use with trusted data sources
- Review generated code before execution in production environments
- Be cautious with user-provided data that could influence code generation

#### Ollama API

The app communicates with Ollama via HTTP:

- Ensure Ollama is running locally (not exposed to the network)
- Use the default localhost configuration
- If using a remote Ollama server, ensure proper network security

### Security Updates

Security updates will be released as patch versions (e.g., 1.0.1, 1.0.2).

To stay informed about security updates:
- Watch the GitHub repository for releases
- Check the CHANGELOG.md for security-related changes

### Responsible Disclosure Timeline

- **Day 0**: Vulnerability reported
- **Day 1-2**: Acknowledgment and initial assessment
- **Day 3-7**: Fix development and testing
- **Day 7-14**: Patch release
- **Day 30**: Public disclosure (if applicable)

## Contact

For security concerns, please contact:
- Email: polcollino@gmail.com
- GitHub Security Advisories: [Create a security advisory](https://github.com/DataConceptz/DataExplorerPro/security/advisories/new)

Thank you for helping keep DataExplorerPro secure!
