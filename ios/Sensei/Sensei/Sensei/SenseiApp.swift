//
//  SenseiApp.swift
//  Sensei
//
//  Created by Arnaud Bailly on 20/01/2026.
//

import SwiftUI

@main
struct SenseiApp: App {
    @StateObject private var settings = SettingsStore()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(settings)
        }
    }
}
