import Foundation
import SwiftUI
import Combine

/// Observable store for user settings, persisted in UserDefaults
final class SettingsStore: ObservableObject {
    private let defaults = UserDefaults.standard
    private var cancellables = Set<AnyCancellable>()

    private enum Keys {
        static let serverURL = "serverURL"
        static let userName = "userName"
        static let flowDir = "flowDir"
        static let cachedUserProfile = "cachedUserProfile"
    }

    /// The Sensei server URL
    @Published var serverURL: String

    /// The logged-in user's name
    @Published var userName: String

    /// The flow directory/context (persisted for convenience)
    @Published var flowDir: String

    /// Cached user profile for offline access
    @Published var cachedProfile: UserProfile?

    /// Whether the user is logged in (has a valid token)
    var isLoggedIn: Bool {
        KeychainHelper.shared.hasToken && !userName.isEmpty
    }

    init() {
        // Initialize with stored values or defaults
        self.serverURL = defaults.string(forKey: Keys.serverURL) ?? "https://sensei.pankzsoft.net"
        self.userName = defaults.string(forKey: Keys.userName) ?? ""
        self.flowDir = defaults.string(forKey: Keys.flowDir) ?? "none"

        // Load cached profile
        if let data = defaults.data(forKey: Keys.cachedUserProfile),
           let profile = try? JSONDecoder().decode(UserProfile.self, from: data) {
            self.cachedProfile = profile
        } else {
            self.cachedProfile = nil
        }

        // Set up persistence observers after initialization
        setupPersistence()
    }

    private func setupPersistence() {
        // Persist serverURL changes
        $serverURL
            .dropFirst() // Skip initial value
            .sink { [weak self] value in
                self?.defaults.set(value, forKey: Keys.serverURL)
            }
            .store(in: &cancellables)

        // Persist userName changes
        $userName
            .dropFirst()
            .sink { [weak self] value in
                self?.defaults.set(value, forKey: Keys.userName)
            }
            .store(in: &cancellables)

        // Persist flowDir changes
        $flowDir
            .dropFirst()
            .sink { [weak self] value in
                self?.defaults.set(value, forKey: Keys.flowDir)
            }
            .store(in: &cancellables)

        // Persist cachedProfile changes
        $cachedProfile
            .dropFirst()
            .sink { [weak self] profile in
                if let profile = profile,
                   let data = try? JSONEncoder().encode(profile) {
                    self?.defaults.set(data, forKey: Keys.cachedUserProfile)
                } else {
                    self?.defaults.removeObject(forKey: Keys.cachedUserProfile)
                }
            }
            .store(in: &cancellables)
    }

    /// Clear all stored data (used on logout)
    func logout() {
        KeychainHelper.shared.deleteToken()
        userName = ""
        cachedProfile = nil
        // Keep serverURL and flowDir for convenience
    }
}
