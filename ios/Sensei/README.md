# Sensei iOS App

A native iOS app for recording flows to your Sensei server.

## Features

- Login with your Sensei credentials
- Configure server URL
- View colorized buttons for each flow type from your profile
- Record flows with a single tap
- Set custom directory/context for flows
- Secure JWT token storage in Keychain

## Requirements

- iOS 15.0+
- Xcode 14.0+

## Setup

### Creating the Xcode Project

1. Open Xcode
2. Create a new project: **File > New > Project**
3. Choose **iOS > App**
4. Configure:
   - Product Name: `SenseiApp`
   - Team: Your development team
   - Organization Identifier: `net.pankzsoft`
   - Interface: **SwiftUI**
   - Language: **Swift**
   - Uncheck "Include Tests" (optional)
5. Save to the `ios/SenseiApp` directory

### Adding Source Files

After creating the project, add the existing Swift files:

1. In Xcode, right-click on the `SenseiApp` folder in the navigator
2. Select **Add Files to "SenseiApp"...**
3. Navigate to and select all `.swift` files in:
   - `SenseiApp/` (SenseiApp.swift, ContentView.swift)
   - `SenseiApp/Models/`
   - `SenseiApp/Services/`
   - `SenseiApp/Views/`
4. Ensure "Copy items if needed" is unchecked (files are already in place)
5. Click **Add**

### Project Configuration

1. Select the project in the navigator
2. Select the `SenseiApp` target
3. Under **General**:
   - Set Deployment Target to iOS 15.0
   - Set Bundle Identifier to `net.pankzsoft.SenseiApp`
4. Under **Signing & Capabilities**:
   - Add **Keychain Sharing** capability (for secure token storage)
   - Set Keychain Groups to `$(AppIdentifierPrefix)net.pankzsoft.sensei`

## Project Structure

```
SenseiApp/
├── SenseiApp.swift          # App entry point
├── ContentView.swift        # Main navigation
├── Models/
│   ├── UserProfile.swift    # User profile model
│   ├── Event.swift          # Flow event model
│   └── Credentials.swift    # Login credentials
├── Services/
│   ├── APIClient.swift      # HTTP client with auth
│   ├── KeychainHelper.swift # Secure token storage
│   └── SettingsStore.swift  # UserDefaults wrapper
├── Views/
│   ├── LoginView.swift      # Login screen
│   ├── RecordView.swift     # Flow buttons grid
│   └── SettingsView.swift   # Settings screen
└── Assets.xcassets/         # App icons and colors
```

## Usage

1. Launch the app
2. Tap the server URL to configure your Sensei server address
3. Enter your username and password
4. Tap **Log In**
5. You'll see a grid of colorized buttons for each flow type
6. Optionally set the Directory/Context field
7. Tap a flow type button to record it

## API Endpoints Used

- `POST /login` - Authenticate with credentials
- `GET /api/users/{user}/token` - Retrieve JWT token
- `GET /api/users` - Fetch user profile
- `POST /api/log/{user}` - Post flow events

## Security

- JWT tokens are stored securely in the iOS Keychain
- Tokens persist across app restarts
- On logout, tokens are deleted from Keychain
- All API requests use HTTPS

## Troubleshooting

### "Authentication required" error
Your session may have expired. Log out and log in again.

### Buttons not showing
Pull down to refresh the profile, or check that your user profile has flow types configured.

### Network errors
Ensure the server URL is correct and the server is reachable.
