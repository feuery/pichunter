//
//  pichunter_http.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 12.11.2023.
//

import Foundation

enum Method: String {
    
    case GET = "get"
    case POST = "post"
    case DELETE  = "delete "
}

func request<RESULT_T>( method: Method, url: String, onError: @escaping (Error) -> (), onReceive: @escaping (RESULT_T) -> ()) where RESULT_T: Codable {
    guard let endpointUrl = URL(string: url) else {
        return
    }
    
    do {
        var request = URLRequest(url: endpointUrl)
        request.httpMethod = method.rawValue

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
                print ("Response for request was \(realResponse.statusCode)")
                return
            }
            
            let dec = JSONDecoder()
            
            do {
                let value = try dec.decode(RESULT_T.self, from: actualData);
                
                DispatchQueue.main.async {
                    if let actual_error = error {
                        onError(actual_error)
                    } else {
                        onReceive(value);
                    }
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

func request<RESULT_T, BODY_T>( method: Method, url: String, body: BODY_T?, onError: @escaping (Error) -> (), onReceive: @escaping (RESULT_T) -> ()) where BODY_T: Codable, RESULT_T: Codable {
    guard let endpointUrl = URL(string: url) else {
        return
    }
    
    do {
        let dec = JSONEncoder();
        
        
        var request = URLRequest(url: endpointUrl)
        request.httpMethod = method.rawValue
        if method != .GET {
            if let BODY = body {
                let data = try dec.encode(BODY)
                request.httpBody = data
            }
        }
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
                print ("Response for request was \(realResponse.statusCode)")
                return
            }
            
            let dec = JSONDecoder()
            
            do {
                // let logged_in_user = try dec.decode(User.self, from: actualData)
                let value = try dec.decode(RESULT_T.self, from: actualData);
                
                DispatchQueue.main.async {
                    if let actual_error = error {
                        onError(actual_error)
                    } else {
                        onReceive(value);
                    }
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
