var app = Elm.Main.init({
    node: document.getElementById("app")
});


app.ports.alert.subscribe( (prompt) => {
    window.alert(prompt);
})
