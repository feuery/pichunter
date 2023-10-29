//
//  pichunterApp.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 6.10.2023.
//

import SwiftUI

class pichunterState: ObservableObject {
    static var State = pichunterState()

    @Published var logged_in = false
    @Published var server_url = ""
    @Published var error_message: String? = nil
    

    
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
    
    func LogIn (username: String, password: String) {
        let url = pichunterState.State.server_url + "/api/login"
        guard let endpointUrl = URL(string: url) else {
            return
        }

        var json = [String:Any]()
        json["username"] = username
        json["password"] = password
        do {
            let data = try JSONSerialization.data(withJSONObject: json, options: [])
            
            var request = URLRequest(url: endpointUrl)
            request.httpMethod = "POST"
            request.httpBody = data
            request.addValue("application/json", forHTTPHeaderField: "Content-Type")
            request.addValue("application/json", forHTTPHeaderField: "Accept")
            
            let session = URLSession(configuration: .default)
            
            let task = session.dataTask(with: request) { (data, response, error) in
                DispatchQueue.main.async {
                    if let actual_error = error {
                        state.error_message = actual_error.localizedDescription
                        return
                    }
                    
                    print("Login successful: \(error), \(response), \(data)")
                    state.logged_in = true
                }
            }
            task.resume()
            
        } catch  {
            print("login failed")
        }
    }
    
    var body: some Scene {
        WindowGroup {
            if state.logged_in {
                HomeScreen(app: self)
            }
            else {
                LoginView(app: self)
            }
        }
    }
}
