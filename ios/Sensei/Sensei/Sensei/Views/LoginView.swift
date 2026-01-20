import SwiftUI

struct LoginView: View {
    @EnvironmentObject var settings: SettingsStore
    private let api = APIClient.shared

    @State private var username = ""
    @State private var password = ""
    @State private var isLoading = false
    @State private var errorMessage: String?
    @State private var showServerSettings = false

    var body: some View {
        VStack(spacing: 24) {
            Spacer()

            // App icon/title
            VStack(spacing: 8) {
                Image(systemName: "clock.arrow.circlepath")
                    .font(.system(size: 60))
                    .foregroundColor(.blue)

                Text("Sensei")
                    .font(.largeTitle)
                    .fontWeight(.bold)
            }

            Spacer()

            // Server URL button
            Button {
                showServerSettings = true
            } label: {
                HStack {
                    Image(systemName: "server.rack")
                    Text(settings.serverURL)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
                .font(.caption)
                .foregroundColor(.secondary)
            }

            // Login form
            VStack(spacing: 16) {
                TextField("Username", text: $username)
                    .textFieldStyle(.roundedBorder)
                    .textContentType(.username)
                    .autocapitalization(.none)
                    .autocorrectionDisabled()

                SecureField("Password", text: $password)
                    .textFieldStyle(.roundedBorder)
                    .textContentType(.password)

                if let error = errorMessage {
                    Text(error)
                        .font(.caption)
                        .foregroundColor(.red)
                        .multilineTextAlignment(.center)
                }

                Button {
                    Task {
                        await login()
                    }
                } label: {
                    if isLoading {
                        ProgressView()
                            .frame(maxWidth: .infinity)
                    } else {
                        Text("Log In")
                            .frame(maxWidth: .infinity)
                    }
                }
                .buttonStyle(.borderedProminent)
                .disabled(username.isEmpty || password.isEmpty || isLoading)
            }
            .padding(.horizontal, 32)

            Spacer()
        }
        .sheet(isPresented: $showServerSettings) {
            ServerSettingsSheet(serverURL: $settings.serverURL)
        }
    }

    private func login() async {
        isLoading = true
        errorMessage = nil

        do {
            let credentials = Credentials(username: username, password: password)
            let profile = try await api.login(credentials: credentials, serverURL: settings.serverURL)

            // Store user info
            settings.userName = profile.userName
            settings.cachedProfile = profile

        } catch let error as APIError {
            errorMessage = error.localizedDescription
        } catch {
            errorMessage = error.localizedDescription
        }

        isLoading = false
    }
}

// MARK: - Server Settings Sheet

struct ServerSettingsSheet: View {
    @Binding var serverURL: String
    @Environment(\.dismiss) private var dismiss

    @State private var editedURL: String = ""

    var body: some View {
        NavigationView {
            Form {
                Section {
                    TextField("Server URL", text: $editedURL)
                        .keyboardType(.URL)
                        .autocapitalization(.none)
                        .autocorrectionDisabled()
                } header: {
                    Text("Server Address")
                } footer: {
                    Text("Enter the full URL of your Sensei server, e.g., https://sensei.example.com")
                }
            }
            .navigationTitle("Server Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") {
                        dismiss()
                    }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Save") {
                        serverURL = editedURL
                        dismiss()
                    }
                }
            }
        }
        .onAppear {
            editedURL = serverURL
        }
    }
}

#Preview {
    LoginView()
        .environmentObject(SettingsStore())
}
