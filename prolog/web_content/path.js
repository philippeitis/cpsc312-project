function urlParam(key, value) {
    return `${encodeURIComponent(key)}=${encodeURIComponent(value)}`;
}

function searchAndDisplay() {
    const form = document
        .getElementById("path_form");

    const oldFormData = new FormData(form);
    const formData = [];

    for (const pair of oldFormData.entries()) {
        if (pair[0].startsWith("discard")) {
            continue;
        }
        if (pair[0] === "inputs" || pair[0] === "outputs") {
            console.log("inputs", pair[1].split(","));
            for (const item of pair[1].split(",")) {
                if (!item.trim()) {
                    continue;
                }
                formData.push(urlParam(pair[0], item.trim()));
            }
        } else {
            if (!pair[1]) {
                continue;
            }
            formData.push(urlParam(pair[0], pair[1]));
        }
    }

    for (const pair of formData.entries()) {
        console.log(pair[0]+ ', >' + pair[1] + "<"); 
    }

    const url = form.action + "?" + formData.join("&");
    console.log(url);

    const xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {
            const json = JSON.parse(this.responseText);
            if (this.status === 200) {
                renderFunction(json);
            }
            if (this.status === 404) {
                document.getElementById("path_msg").innerHTML = "<h2 style='color:red'>" + json["msg"] + "</h2>";
                document.getElementById("paths").innerHTML = "";
            }
        }
    };

    xmlhttp.open("GET", url, true);
    xmlhttp.send();

    function renderFunction(json) {
        const uuidMap = new Map();
        json["funcs"].forEach(function (x) {
            uuidMap.set(x["uuid"], x);
        });
        console.log(uuidMap);
        document.getElementById("path_msg").innerHTML = "<h2>" + json["msg"] + "</h2>";
        let output = "";
        for (let i = 0; i < json["paths"].length; i++) {
            output += "<p>" + json["paths"][i].map((x) => (uuidMap.get(x).name))
                .join(" -> ") + "<\p><br>";
        }
        document.getElementById("paths").innerHTML = output;
    }
}

function clearResults() {
    document.getElementById("path_msg").innerHTML = "";
    document.getElementById("paths").innerHTML = "";
}

function updatePathLen(updated) {
    const pathLenSlider = document.getElementById("path_length");
    const pathLenText = document.getElementById("path_length_t");
    if (updated === "slider") {
        pathLenText.value = pathLenSlider.value;
    } else {
        pathLenSlider.value = pathLenText.value;
    }
}