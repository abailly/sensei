import Foundation
import Security

/// Helper class for securely storing and retrieving the JWT token from Keychain
final class KeychainHelper {
    static let shared = KeychainHelper()

    private let service = "net.pankzsoft.sensei"
    private let account = "jwt-token"

    private init() {}

    /// Save the JWT token to Keychain
    /// - Parameter token: The JWT token string to save
    func saveToken(_ token: String) {
        guard let data = token.data(using: .utf8) else { return }

        // Delete any existing token first
        deleteToken()

        let query: [String: Any] = [
            kSecClass as String: kSecClassGenericPassword,
            kSecAttrService as String: service,
            kSecAttrAccount as String: account,
            kSecValueData as String: data,
            kSecAttrAccessible as String: kSecAttrAccessibleAfterFirstUnlock
        ]

        let status = SecItemAdd(query as CFDictionary, nil)
        if status != errSecSuccess {
            print("KeychainHelper: Failed to save token, status: \(status)")
        }
    }

    /// Retrieve the JWT token from Keychain
    /// - Returns: The JWT token string, or nil if not found
    func getToken() -> String? {
        let query: [String: Any] = [
            kSecClass as String: kSecClassGenericPassword,
            kSecAttrService as String: service,
            kSecAttrAccount as String: account,
            kSecReturnData as String: true,
            kSecMatchLimit as String: kSecMatchLimitOne
        ]

        var result: AnyObject?
        let status = SecItemCopyMatching(query as CFDictionary, &result)

        guard status == errSecSuccess,
              let data = result as? Data,
              let token = String(data: data, encoding: .utf8) else {
            return nil
        }

        return token
    }

    /// Delete the JWT token from Keychain
    func deleteToken() {
        let query: [String: Any] = [
            kSecClass as String: kSecClassGenericPassword,
            kSecAttrService as String: service,
            kSecAttrAccount as String: account
        ]

        SecItemDelete(query as CFDictionary)
    }

    /// Check if a token exists in Keychain
    var hasToken: Bool {
        getToken() != nil
    }
}
