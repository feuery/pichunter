//
//  pichunterApp.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 6.10.2023.
//

import SwiftUI

class pichunterState: ObservableObject {
    static var State = pichunterState()

    @Published var server_url = ""
    @Published var error_message: String? = nil
    @Published var logged_in_user: User? = nil
    
    @Published var picCounts = [Int: PictureCount]()

    
    private init() {
    }
}

@main
struct pichunterApp: App {
    
    @ObservedObject var state = pichunterState.State
    
    init() {
        pichunterState.State.server_url = userdefaults.string(forKey: "server_url") ?? "http://localhost:3000"
    }
    
    let userdefaults = UserDefaults()
    
    func errorHandler(actual_error: Error) {
        state.error_message = actual_error.localizedDescription
    }
    
    func loadCodesets_and_counts() {
        
        let url = pichunterState.State.server_url + "/pictures/count-per-county"
        request(method: .GET, url: url, onError: errorHandler) { (highscores: [PictureCount]) in
            state.picCounts = highscores.reduce(into: [Int: PictureCount]()) {
                $0[$1.county] = $1
            }
        }
        
        
    }
    func LogIn (username: String, password: String) {
        let url = pichunterState.State.server_url + "/api/login"
        let usr = Login_User(username: username, password: password)
        
        request(method: .POST, url: url, body: usr,
                onError: errorHandler) { (logged_in_user: User) in
            state.logged_in_user = logged_in_user
        }
    }
    
    var body: some Scene {
        WindowGroup {
            if state.logged_in_user != nil {
                HomeScreen()
            }
            else {
                LoginView(app: self)
            }
        }
    }
}
