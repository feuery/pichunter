var app = Elm.Main.init({
    node: document.getElementById("app")
});


app.ports.alert.subscribe( (prompt) => {
    window.alert(prompt);
});

function initmap (id, lat, long) {
    let map = L.map(id).setView([lat, long], 13);
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	maxZoom: 19,
	attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    return map;
}

app.ports.initializeAdminMaps.subscribe( ids => {
    ids.forEach( ([id, lat, long]) =>
	{
	    document.arrive('#'+id, function(element) {
		let map = initmap(id, lat, long);
		let marker = L.marker([lat, long]).addTo(map);
	    });
	});
});

app.ports.initGameMap.subscribe(triple => {
    let [id, lat, long] = triple;
    document.arrive('#'+id, elm => {
	let map = initmap(id, lat, long);
	map.on('click', e => {
	    let distance = e.latlng.distanceTo( L.latLng([lat, long]))
	    
	    app.ports.mapClicked.send (distance);
	});
    });
});
