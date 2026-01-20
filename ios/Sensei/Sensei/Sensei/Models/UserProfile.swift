import Foundation
import SwiftUI

/// User profile as returned by the Sensei API
struct UserProfile: Codable {
    let userName: String
    let userTimezone: String
    let userStartOfDay: String
    let userEndOfDay: String
    let userFlowTypes: [String: String]?
    let userCommands: [String: String]?
    let userProjects: [String: String]?
    let userProfileVersion: Int?

    /// Returns flow types with their colors, or default types if none configured
    var flowTypes: [(name: String, color: Color)] {
        if let types = userFlowTypes, !types.isEmpty {
            return types.map { (name: $0.key, color: Color(hex: $0.value)) }
                .sorted { $0.name < $1.name }
        } else {
            return Self.defaultFlowTypes
        }
    }

    /// Default flow types when user has none configured
    static let defaultFlowTypes: [(name: String, color: Color)] = [
        ("Experimenting", Color(hex: "0x0022dd")),
        ("Flowing", Color(hex: "0x00dd22")),
        ("Learning", Color(hex: "0xff8822")),
        ("Meeting", Color(hex: "0xfff203")),
        ("Rework", Color(hex: "0x4500dd")),
        ("Troubleshooting", Color(hex: "0xee1111"))
    ]
}

// MARK: - Color Extension for Hex Parsing

extension Color {
    /// Initialize a Color from a hex string like "0xRRGGBB" or "#RRGGBB"
    init(hex: String) {
        var hexSanitized = hex.trimmingCharacters(in: .whitespacesAndNewlines)

        // Remove prefix
        if hexSanitized.hasPrefix("0x") {
            hexSanitized = String(hexSanitized.dropFirst(2))
        } else if hexSanitized.hasPrefix("#") {
            hexSanitized = String(hexSanitized.dropFirst())
        }

        // Parse hex value
        var rgbValue: UInt64 = 0
        Scanner(string: hexSanitized).scanHexInt64(&rgbValue)

        let red = Double((rgbValue & 0xFF0000) >> 16) / 255.0
        let green = Double((rgbValue & 0x00FF00) >> 8) / 255.0
        let blue = Double(rgbValue & 0x0000FF) / 255.0

        self.init(red: red, green: green, blue: blue)
    }
}
