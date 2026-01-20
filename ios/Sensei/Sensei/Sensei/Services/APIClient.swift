import Foundation

/// API client errors
enum APIError: Error, LocalizedError {
    case invalidURL
    case unauthorized
    case serverError(Int)
    case networkError(Error)
    case decodingError(Error)
    case unknownError

    var errorDescription: String? {
        switch self {
        case .invalidURL:
            return "Invalid server URL"
        case .unauthorized:
            return "Authentication required. Please log in."
        case .serverError(let code):
            return "Server error (HTTP \(code))"
        case .networkError(let error):
            return "Network error: \(error.localizedDescription)"
        case .decodingError(let error):
            return "Data error: \(error.localizedDescription)"
        case .unknownError:
            return "An unknown error occurred"
        }
    }
}

/// API client for communicating with the Sensei server
final class APIClient {
    static let shared = APIClient()

    /// API version sent with all requests
    private static let apiVersion = "0.45.2"

    private let session: URLSession
    private let keychain = KeychainHelper.shared

    private init() {
        let config = URLSessionConfiguration.default
        config.httpShouldSetCookies = true
        config.httpCookieAcceptPolicy = .always
        self.session = URLSession(configuration: config)
    }

    /// Set common headers on a request
    private func setCommonHeaders(_ request: inout URLRequest) {
        request.setValue(Self.apiVersion, forHTTPHeaderField: "X-API-Version")
    }

    // MARK: - Authentication

    /// Log in with credentials and retrieve JWT token
    /// - Parameters:
    ///   - credentials: User credentials
    ///   - serverURL: The server base URL
    /// - Returns: The user profile
    func login(credentials: Credentials, serverURL: String) async throws -> UserProfile {
        // Step 1: POST to /login to authenticate and get cookies
        guard let loginURL = URL(string: "\(serverURL)/login") else {
            throw APIError.invalidURL
        }

        var loginRequest = URLRequest(url: loginURL)
        loginRequest.httpMethod = "POST"
        loginRequest.setValue("application/json", forHTTPHeaderField: "Content-Type")
        setCommonHeaders(&loginRequest)
        loginRequest.httpBody = try JSONEncoder().encode(credentials)

        let (loginData, loginResponse) = try await session.data(for: loginRequest)

        guard let httpResponse = loginResponse as? HTTPURLResponse else {
            throw APIError.unknownError
        }

        guard httpResponse.statusCode == 200 else {
            if httpResponse.statusCode == 401 {
                throw APIError.unauthorized
            }
            throw APIError.serverError(httpResponse.statusCode)
        }

        // Decode user profile from login response
        let profile: UserProfile
        do {
            profile = try JSONDecoder().decode(UserProfile.self, from: loginData)
        } catch {
            throw APIError.decodingError(error)
        }

        // Step 2: GET /api/users/{userName}/token to retrieve JWT token
        guard let tokenURL = URL(string: "\(serverURL)/api/users/\(credentials.credLogin)/token") else {
            throw APIError.invalidURL
        }

        var tokenRequest = URLRequest(url: tokenURL)
        tokenRequest.httpMethod = "GET"
        setCommonHeaders(&tokenRequest)
        // Cookies from login are automatically sent

        let (tokenData, tokenResponse) = try await session.data(for: tokenRequest)

        guard let tokenHttpResponse = tokenResponse as? HTTPURLResponse,
              tokenHttpResponse.statusCode == 200 else {
            throw APIError.serverError((tokenResponse as? HTTPURLResponse)?.statusCode ?? 0)
        }

        // Token is returned as a JSON string
        if let token = try? JSONDecoder().decode(String.self, from: tokenData) {
            keychain.saveToken(token)
        } else if let tokenString = String(data: tokenData, encoding: .utf8)?
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .trimmingCharacters(in: CharacterSet(charactersIn: "\"")) {
            keychain.saveToken(tokenString)
        }

        return profile
    }

    // MARK: - User Profile

    /// Fetch the current user's profile
    /// - Parameter serverURL: The server base URL
    /// - Returns: The user profile
    func getUserProfile(serverURL: String) async throws -> UserProfile {
        guard let url = URL(string: "\(serverURL)/api/users") else {
            throw APIError.invalidURL
        }

        let request = try authenticatedRequest(url: url, method: "GET")
        let (data, response) = try await session.data(for: request)

        try checkResponse(response)

        do {
            return try JSONDecoder().decode(UserProfile.self, from: data)
        } catch {
            throw APIError.decodingError(error)
        }
    }

    // MARK: - Flow Recording

    /// Post a flow event
    /// - Parameters:
    ///   - flowType: The type of flow to record
    ///   - userName: The user name
    ///   - flowDir: The directory/context
    ///   - serverURL: The server base URL
    func postFlow(flowType: String, userName: String, flowDir: String, serverURL: String) async throws {
        guard let url = URL(string: "\(serverURL)/api/log/\(userName)") else {
            throw APIError.invalidURL
        }

        let event = FlowEvent(flowType: flowType, userName: userName, flowDir: flowDir)

        var request = try authenticatedRequest(url: url, method: "POST")
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode([event])

        let (_, response) = try await session.data(for: request)
        try checkResponse(response)
    }

    // MARK: - Helpers

    /// Create an authenticated request with Bearer token
    private func authenticatedRequest(url: URL, method: String) throws -> URLRequest {
        guard let token = keychain.getToken() else {
            throw APIError.unauthorized
        }

        var request = URLRequest(url: url)
        request.httpMethod = method
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        setCommonHeaders(&request)
        return request
    }

    /// Check HTTP response for errors
    private func checkResponse(_ response: URLResponse) throws {
        guard let httpResponse = response as? HTTPURLResponse else {
            throw APIError.unknownError
        }

        switch httpResponse.statusCode {
        case 200...299:
            return
        case 401:
            keychain.deleteToken()
            throw APIError.unauthorized
        default:
            throw APIError.serverError(httpResponse.statusCode)
        }
    }
}
