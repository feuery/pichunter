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
    @State var remember_login: Bool

    @StateObject var state = pichunterState.State;
    
    
    init(app: pichunterApp? = nil) {
        self.app = app
        
        if let realApp = app {
            self.username = realApp.userdefaults.string(forKey: "username") ?? ""
            self.remember_login = realApp.userdefaults.bool(forKey: "remember_login")
        } else {
            self.username = ""
            self.remember_login = false
        }
    }

    
    var body: some View {
        if let realApp = app {
            VStack (alignment: /*@START_MENU_TOKEN@*/.center/*@END_MENU_TOKEN@*/) {
                TextField("Username", text: $username)
                SecureField("Password", text: $password)
                
                Button("Log in!") {
                        realApp.LogIn(username: username, password: password)
                    }
                
                
                Divider()
                
                Label {
                    Text("Server we're connecting to: ")
                } icon: {}
                
                    TextField("Server", text: $state.server_url)
            }
            .padding()
        }
    }
}

#Preview {
    LoginView()
}
