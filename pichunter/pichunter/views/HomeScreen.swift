//
//  HomeScreen.swift
//  pichunter
//
//  Created by Ilpo Lehtinen on 7.10.2023.
//

import SwiftUI

struct HomeScreen: View {
    
    var app: pichunterApp?
    
    init(app: pichunterApp? = nil) {
        self.app = app
    }
    
    var body: some View {
        if let realApp = app {
            Text("Tervetuloa kotinäytölle!")
            Text(pichunterState.State.error_message ?? "")
        }
    }
}

#Preview {
    HomeScreen()
}
