import SwiftUI

struct RecordView: View {
    @EnvironmentObject var settings: SettingsStore
    private let api = APIClient.shared

    @State private var isLoading = false
    @State private var recordingFlowType: String?
    @State private var showSuccess = false
    @State private var successMessage = ""
    @State private var errorMessage: String?

    private let columns = [
        GridItem(.adaptive(minimum: 120), spacing: 12)
    ]

    var body: some View {
        ScrollView {
            VStack(spacing: 20) {
                // Flow directory/context field
                VStack(alignment: .leading, spacing: 8) {
                    Text("Directory / Context")
                        .font(.caption)
                        .foregroundColor(.secondary)

                    TextField("e.g., project name or path", text: $settings.flowDir)
                        .textFieldStyle(.roundedBorder)
                        .autocapitalization(.none)
                        .autocorrectionDisabled()
                }
                .padding(.horizontal)

                // Success/Error messages
                if showSuccess {
                    HStack {
                        Image(systemName: "checkmark.circle.fill")
                            .foregroundColor(.green)
                        Text(successMessage)
                    }
                    .padding()
                    .background(Color.green.opacity(0.1))
                    .cornerRadius(8)
                    .padding(.horizontal)
                }

                if let error = errorMessage {
                    HStack {
                        Image(systemName: "exclamationmark.triangle.fill")
                            .foregroundColor(.red)
                        Text(error)
                    }
                    .padding()
                    .background(Color.red.opacity(0.1))
                    .cornerRadius(8)
                    .padding(.horizontal)
                }

                // Flow type buttons grid
                LazyVGrid(columns: columns, spacing: 12) {
                    ForEach(flowTypes, id: \.name) { flowType in
                        FlowButton(
                            name: flowType.name,
                            color: flowType.color,
                            isLoading: recordingFlowType == flowType.name
                        ) {
                            Task {
                                await recordFlow(flowType.name)
                            }
                        }
                        .disabled(recordingFlowType != nil)
                    }
                }
                .padding(.horizontal)

                Spacer(minLength: 20)
            }
            .padding(.top)
        }
        .refreshable {
            await refreshProfile()
        }
        .onAppear {
            // Refresh profile on appear if we don't have cached data
            if settings.cachedProfile == nil {
                Task {
                    await refreshProfile()
                }
            }
        }
    }

    // MARK: - Computed Properties

    private var flowTypes: [(name: String, color: Color)] {
        var types = settings.cachedProfile?.flowTypes ?? UserProfile.defaultFlowTypes
        // Always add "End" flow type at the end
        types.append((name: "End", color: .gray))
        return types
    }

    // MARK: - Actions

    private func recordFlow(_ flowType: String) async {
        recordingFlowType = flowType
        errorMessage = nil
        showSuccess = false

        do {
            try await api.postFlow(
                flowType: flowType,
                userName: settings.userName,
                flowDir: settings.flowDir.isEmpty ? "none" : settings.flowDir,
                serverURL: settings.serverURL
            )

            successMessage = "Recorded: \(flowType)"
            showSuccess = true

            // Hide success message after 3 seconds
            DispatchQueue.main.asyncAfter(deadline: .now() + 3) {
                showSuccess = false
            }

        } catch let error as APIError {
            errorMessage = error.localizedDescription

            // If unauthorized, trigger re-login
            if case .unauthorized = error {
                settings.logout()
            }
        } catch {
            errorMessage = error.localizedDescription
        }

        recordingFlowType = nil
    }

    private func refreshProfile() async {
        do {
            let profile = try await api.getUserProfile(serverURL: settings.serverURL)
            settings.cachedProfile = profile
        } catch let error as APIError {
            if case .unauthorized = error {
                settings.logout()
            }
        } catch {
            // Silently fail on refresh - we still have cached data
        }
    }
}

// MARK: - Flow Button

struct FlowButton: View {
    let name: String
    let color: Color
    let isLoading: Bool
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            ZStack {
                RoundedRectangle(cornerRadius: 12)
                    .fill(color)
                    .shadow(color: .black.opacity(0.2), radius: 2, x: 0, y: 2)

                if isLoading {
                    ProgressView()
                        .progressViewStyle(CircularProgressViewStyle(tint: .white))
                } else {
                    Text(name)
                        .font(.headline)
                        .fontWeight(.semibold)
                        .foregroundColor(.white)
                        .shadow(color: .black.opacity(0.3), radius: 1, x: 0, y: 1)
                        .multilineTextAlignment(.center)
                        .padding(.horizontal, 8)
                }
            }
        }
        .frame(minHeight: 80)
        .buttonStyle(.plain)
    }
}

#Preview {
    NavigationView {
        RecordView()
            .navigationTitle("Record Flow")
    }
    .environmentObject(SettingsStore())
}
