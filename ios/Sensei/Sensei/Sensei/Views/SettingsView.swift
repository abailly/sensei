import SwiftUI

struct SettingsView: View {
    @EnvironmentObject var settings: SettingsStore
    @Environment(\.dismiss) private var dismiss

    @State private var showLogoutConfirmation = false

    var body: some View {
        NavigationView {
            Form {
                // User section
                Section {
                    HStack {
                        Text("Username")
                        Spacer()
                        Text(settings.userName)
                            .foregroundColor(.secondary)
                    }

                    if let profile = settings.cachedProfile {
                        HStack {
                            Text("Timezone")
                            Spacer()
                            Text(profile.userTimezone)
                                .foregroundColor(.secondary)
                        }

                        HStack {
                            Text("Flow Types")
                            Spacer()
                            Text("\(profile.flowTypes.count)")
                                .foregroundColor(.secondary)
                        }
                    }
                } header: {
                    Text("Account")
                }

                // Server section
                Section {
                    HStack {
                        Text("Server")
                        Spacer()
                        Text(settings.serverURL)
                            .foregroundColor(.secondary)
                            .lineLimit(1)
                            .truncationMode(.middle)
                    }
                } header: {
                    Text("Server")
                } footer: {
                    Text("Server URL can only be changed before logging in.")
                }

                // Default flowDir section
                Section {
                    TextField("Directory / Context", text: $settings.flowDir)
                        .autocapitalization(.none)
                        .autocorrectionDisabled()
                } header: {
                    Text("Default Directory")
                } footer: {
                    Text("This value will be sent with each flow you record.")
                }

                // Logout section
                Section {
                    Button(role: .destructive) {
                        showLogoutConfirmation = true
                    } label: {
                        HStack {
                            Spacer()
                            Text("Log Out")
                            Spacer()
                        }
                    }
                }

                // App info section
                Section {
                    HStack {
                        Text("Version")
                        Spacer()
                        Text(Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "1.0")
                            .foregroundColor(.secondary)
                    }
                } header: {
                    Text("About")
                }
            }
            .navigationTitle("Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button("Done") {
                        dismiss()
                    }
                }
            }
            .confirmationDialog(
                "Are you sure you want to log out?",
                isPresented: $showLogoutConfirmation,
                titleVisibility: .visible
            ) {
                Button("Log Out", role: .destructive) {
                    settings.logout()
                    dismiss()
                }
                Button("Cancel", role: .cancel) {}
            }
        }
    }
}

#Preview {
    SettingsView()
        .environmentObject(SettingsStore())
}
