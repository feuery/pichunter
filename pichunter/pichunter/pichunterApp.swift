//
//  pichunterApp.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 6.10.2023.
//

import SwiftUI

@main
struct pichunterApp: App {
    
    @State var logged_in = false
    
    static var app: pichunterApp?
    
    init() {
        if pichunterApp.app == nil {
            pichunterApp.app = self
        }
    }
    
    let userdefaults = UserDefaults()
    
    func LogIn (username: String, password: String, server:String) {
        print("Doing login with params: " + username + ", " + password + ", " + server)
        logged_in = true
    }
    
    var body: some Scene {
        WindowGroup {
            if logged_in {
                HomeScreen()
            }
            else {
                LoginView(app: self)
            }
        }
    }
}
