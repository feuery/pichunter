//
//  HomeScreen.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 7.10.2023.
//

import SwiftUI
import Foundation

struct HomeHeader: View {
    let user: User?
    @StateObject var state = pichunterState.State;
    
    var body: some View {
        if let usr = user {
            GridRow {
                AsyncImage(url: URL(string: state.server_url +  "/api/avatar/" + usr.imgId))
                Text(usr.displayName + " (" + usr.username + ")")
            }
        }
        else {
            Text("No user logged in")
        }
    }
}


struct HomeBody: View {
    var body: some View {
        GridRow {
            Button("Place pictures on the map")
            {
                print("Placing shit on map")
            }
            .bold()
            Text("Started playing on 3.10.2023 with score 1/2")
        }
        GridRow {
            Button("Show pictures from places")
            {
                print("Posting pictures from places")
            }
            .bold()
            Text("Started playing on 3.8.2023 with score 11/21")
        }
        GridRow {
            Text("Your highest scores:")
            VStack() {
                HStack() {
                    Text("Location").bold()
                    Text("1/2")
                }
                HStack {
                    Text("Pictures").bold()
                    Text("11/21")
                }
            }
        }
    }
}
 
struct HomeScreen: View {
    @StateObject var state = pichunterState.State;
    
    var body: some View {
            Grid(alignment: .top) {
                HomeHeader(user: state.logged_in_user)
                .frame(maxWidth: /*@START_MENU_TOKEN@*/.infinity/*@END_MENU_TOKEN@*/, maxHeight: .infinity)
.frame(maxWidth: /*@START_MENU_TOKEN@*/.infinity/*@END_MENU_TOKEN@*/, maxHeight: .infinity)
                HomeBody().frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            .frame(maxWidth: /*@START_MENU_TOKEN@*/.infinity/*@END_MENU_TOKEN@*/, maxHeight: .infinity)
        }
}

#Preview {
    HomeScreen()
}
