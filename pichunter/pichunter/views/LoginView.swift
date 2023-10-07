//
//  ContentView.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 6.10.2023.
//

import SwiftUI

struct LoginView: View {
    
    let app: pichunterApp?
    
    @State var username: String
    @State var password = ""
    @State var server_url: String
    @State var remember_login: Bool
    
    
    init(app: pichunterApp?) {
        self.app = app
        
        if let realApp = app {
            self.username = realApp.userdefaults.string(forKey: "username") ?? ""
            self.server_url  = realApp.userdefaults.string(forKey: "server_url") ?? "http://localhost:3000"
            self.remember_login = realApp.userdefaults.bool(forKey: "remember_login")
        } else {
            self.username = ""
            self.server_url = "http://localhost:3000"
            self.remember_login = false
        }
        
    }

    
    var body: some View {
        VStack (alignment: /*@START_MENU_TOKEN@*/.center/*@END_MENU_TOKEN@*/) {
            TextField("Username", text: $username)
            SecureField("Password", text: $password)
            
            Button("Log in!") {
                if let realApp = app {
                    realApp.LogIn(username: username, password: password, server: server_url)
                }
            }
            
            Divider()
            
            Label {
                Text("Server we're connecting to: ")
            } icon: {}
            
            TextField("Server", text: $server_url)
        }
        .padding()
    }
}

#Preview {
    LoginView(app: pichunterApp.app)
}
