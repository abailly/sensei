import Foundation

/// Flow event to be posted to the Sensei API
struct FlowEvent: Codable {
    let tag: String
    let version: Int
    let flowType: String
    let flowUser: String
    let flowTimestamp: String
    let flowDir: String

    /// Create a new flow event
    /// - Parameters:
    ///   - flowType: The type of flow (e.g., "Flowing", "Meeting")
    ///   - userName: The user name
    ///   - flowDir: The directory/context for the flow
    init(flowType: String, userName: String, flowDir: String) {
        self.tag = "Flow"
        self.version = 13  // Current Sensei storage version
        self.flowType = flowType
        self.flowUser = userName
        self.flowTimestamp = ISO8601DateFormatter().string(from: Date())
        self.flowDir = flowDir
    }
}
