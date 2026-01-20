import Foundation

/// Login credentials for the Sensei API
struct Credentials: Codable {
    let credLogin: String
    let credPassword: String

    init(username: String, password: String) {
        self.credLogin = username
        self.credPassword = password
    }
}
