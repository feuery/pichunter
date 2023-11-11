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
                
                
                guard let realResponse = response as? HTTPURLResponse else {
                    print("Can't cast response as? HTTPURLResponse")
                    return
                }
                
                guard let actualData = data else {
                    print("Didn't receive reponse data from login")
                    return
                }
                
                guard realResponse.statusCode == 200 else {
                    print ("Response for login request was \(realResponse.statusCode)")
                    return
                }
                
                guard let jsonString = String(data: actualData, encoding: .utf8) else {
                    print("Can't create jsonstring")
                    return
                }
                
                let dec = JSONDecoder()
                
                do {
                    let logged_in_user = try dec.decode(User.self, from: actualData)
                    
                    DispatchQueue.main.async {
                        if let actual_error = error {
                            state.error_message = actual_error.localizedDescription
                            return
                        }
                        
                        
                        
                        print("Login successful: \(jsonString)")
                        state.logged_in_user = logged_in_user
                    }
                }
                catch {
                    print(error)
                    print("decoding user json failed")
                    
                }
            }
            task.resume()
            
        } catch  {
            print("login failed")
        }
    }
    
    var body: some Scene {
        WindowGroup {
            if state.logged_in_user != nil {
                HomeScreen(app: self)
            }
            else {
                LoginView(app: self)
            }
        }
    }
}
