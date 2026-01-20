import SwiftUI

struct ContentView: View {
    @EnvironmentObject var settings: SettingsStore
    @State private var showSettings = false

    var body: some View {
        NavigationView {
            if settings.isLoggedIn {
                RecordView()
                    .navigationTitle("Record Flow")
                    .toolbar {
                        ToolbarItem(placement: .navigationBarTrailing) {
                            Button {
                                showSettings = true
                            } label: {
                                Image(systemName: "gear")
                            }
                        }
                    }
                    .sheet(isPresented: $showSettings) {
                        SettingsView()
                    }
            } else {
                LoginView()
                    .navigationTitle("Sensei")
            }
        }
        .navigationViewStyle(.stack)
    }
}

#Preview {
    ContentView()
        .environmentObject(SettingsStore())
}
