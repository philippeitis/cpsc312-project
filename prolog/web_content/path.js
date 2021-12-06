let oldHash = window.location.hash.slice(1);

document.addEventListener('DOMContentLoaded', init, false);

function init() {
    window.addEventListener("hashchange", highlightLinked, false);
}

function urlParam(key, value) {
    return `${encodeURIComponent(key)}=${encodeURIComponent(value)}`;
}

function renderFunctions(json) {
    let output = "";
    for (let i = 0; i < json["functions"].length; i++) {
        const fn = json["functions"][i];
        let style = "padding: 24px 24px 24px 24px;";
        if (oldHash === fn.uuid) {
            style += "background-color: #FFFF9F;"
        }
        output += `<div id="${fn.uuid}" data-uuid="${fn["uuid"]}" style="${style}">`
        output += `<h3>${fn.name}</h3>`
        output += `<p>${fn.docs}</p>`;
        output += "</div>";
    }
    document.getElementById("functions").innerHTML = output;
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

    const url = form.action + "?" + formData.join("&");
    const xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {
            const json = JSON.parse(this.responseText);
            if (this.status === 200) {
                renderPath(json);
            }
            if (this.status === 404) {
                document.getElementById("path_msg").innerHTML = "<h2 style='color:red'>" + json["msg"] + "</h2>";
                document.getElementById("paths").innerHTML = "";
            }
        }
    };

    xmlhttp.open("GET", url, true);
    xmlhttp.send();

    function renderPath(json) {
        const uuidMap = new Map();
        json["functions"].forEach(function (x) {
            uuidMap.set(x["uuid"], x);
        });
        const msg = document.getElementById("path_msg");

        msg.innerHTML = "<h2>" + json["msg"] + "</h2>";
        let output = "";
        for (const path of json["paths"]) {
            output += "<p>" + path.map((uuid) => (
                `<a href="#${uuid}" class="link-dark">${uuidMap.get(uuid).name}</a>`
            )).join(" -> ") + "<\p><br>";
        }
        document.getElementById("paths").innerHTML = output;
        renderFunctions(json);
        msg.scrollIntoView();
    }
}

function clearResults() {
    document.getElementById("path_msg").innerHTML = "";
    document.getElementById("paths").innerHTML = "";
    document.getElementById("functions").innerHTML = "";
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

function highlightLinked() {
    const highlighted = document.getElementById(window.location.hash.slice(1));

    if (oldHash) {
        const oldHighlighted = document.getElementById(oldHash);
        if (oldHighlighted) {
            oldHighlighted.style.backgroundColor = "#FFFFFF"
        }
    }

    if (highlighted) {
        highlighted.style.backgroundColor = "#FFFF9F";
    }

    oldHash = window.location.hash.slice(1);
}
