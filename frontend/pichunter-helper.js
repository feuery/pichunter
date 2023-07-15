var app = Elm.Main.init({
    node: document.getElementById("app")
});


app.ports.alert.subscribe( (prompt) => {
    window.alert(prompt);
});

app.ports.initializeMaps.subscribe( ids => {
    ids.forEach( ([id, lat, long]) =>
	{
	    document.arrive('#'+id, function(element) {
		let map = L.map(id).setView([long, lat], 13);
		L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
		    maxZoom: 19,
		    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
		}).addTo(map);

		let marker = L.marker([long, lat]).addTo(map);
	    });
	});
});
