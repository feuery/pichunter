//
//  User.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 11.11.2023.
//

import Foundation

/*
 {"id":1,"username":"feuer","displayName":"feuer","imgId":"NULL","abilities":["view-picture","insert-picture","can-admin"],"activated?":true}
 */

struct User: Codable {
    let id: Int
    let username: String
    let displayName: String
    let imgId: String
    let abilities: [String]
    let activated: Bool
    
    private enum CodingKeys : String, CodingKey {
        case id, username, displayName, imgId, abilities, activated = "activated?"
    }
}

struct Login_User: Codable {
    let username: String
    let password: String
}
