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

let map_element = null;

let handleMap = (id, lat, long) => element => {

    let map = initmap("map", lat, long);
    map.on('click', e => {
	let distance = e.latlng.distanceTo( L.latLng([lat, long]))
	
	app.ports.mapClicked.send (distance);
    });

    map_element = map;
};

app.ports.initGameMap.subscribe(triple => {
    let [id, lat, long] = triple;

    if (map_element) {
	map_element.off('click');
	map_element.on('click', e => {
	    let distance = e.latlng.distanceTo( L.latLng([lat, long]))
	    
	    app.ports.mapClicked.send (distance);
	});
    }
    else
    {
	document.arrive("#map", handleMap(id, lat, long));
    };
});

function checkGameFiles (id) {
    let input_el = document.getElementById(id);
    const files = Array.from(input_el.files);

    files.forEach( async function(file) {
	try {
	    const tags = await ExifReader.load(file);

	    if (! tags["GPSLatitude"]) {
		input_el.value = null;
		app.ports.noGpsFound.send(null);
	    }
	}
	catch (ex) {
	    input_el.value = null;
	    app.ports.noGpsFound.send(null);
	}
    });
}    
app.ports.checkGameFiles.subscribe(checkGameFiles);

app.ports.resetInput.subscribe( id => {
    document.getElementById(id).value = null;
});
