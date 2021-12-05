function urlParam(key, value) {
    return `${encodeURIComponent(key)}=${encodeURIComponent(value)}`;
}

function searchAndDisplay() {
    const xmlhttp = new XMLHttpRequest();
    const form = document
        .getElementById("search_form");

    const oldFormData = new FormData(form);
    const formData = [];

    for (var pair of oldFormData.entries()) {
        if (pair[0] === "inputs" || pair[0] === "outputs") {
            console.log("inputs", pair[1].split(","));
            for (var item of pair[1].split(",")) {
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

    for (var pair of formData.entries()) {
        console.log(pair[0]+ ', >' + pair[1] + "<"); 
    }

    const url = form.action + "?" + formData.join("&");
    console.log(url);

    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {
            const json = JSON.parse(this.responseText);
            if (this.status === 200) {
                renderFunction(json);
            }
            if (this.status === 404) {
                document.getElementById("function_msg").innerHTML = "<h2 style='color:red'>" + json["msg"] + "</h2>";
                document.getElementById("functions").innerHTML = "";
            }
        }
    };

    xmlhttp.open("GET", url, true);
    xmlhttp.send();

    function renderFunction(json) {
        document.getElementById("function_msg").innerHTML = "<h2>" + json["msg"] + "</h2>";
        let output = "";
        for (let i = 0; i < json["functions"].length; i++) {
            output += "<div data-uuid=\"" + json["functions"][i]["uuid"] +"\">"
            // function header
            output += "<table><thead><tr><td><h3>";
            output += json["functions"][i]["name"] + "</h3></td><td>";
            output += "<input type=checkbox>";
            output += "<td>&nbsp; &nbsp; <input type=\"image\" src=\"trashbin.png\" style=\"width:16px;height:16px;\" onclick=onClickDelete(\"" + json["functions"][i]["uuid"] + "\") /></td>";
            output += "</td></tr></thead></table>";

            // function paragraph
            output += "<p>" + json["functions"][i]["docs"] + "</p>";
            output += "</div>";
        }
        document.getElementById("functions").innerHTML = output;
        
    }
}

function onClickDelete(uuid) {
    // Remove from HTML
    var elements = document.querySelectorAll("div[data-uuid=\"" + uuid + "\"]");
    console.log(uuid);
    Array.prototype.forEach.call(elements, function(node) {
        node.parentNode.removeChild(node);
    });

    const xmlhttp = new XMLHttpRequest();
    const url = "/func?uuid=" + uuid;
    console.log(url);
    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {

            if (this.status === 200) {
                console.log("Deleted");
            } else if (this.status === 405) {
                console.log("Deleting parent!");
                const json = JSON.parse(this.responseText);
                onClickDelete(json["parent_uuid"]);
            }
        }
    };

    xmlhttp.open("DELETE", url, true);
    xmlhttp.send();
}

function clearResults() {
    document.getElementById("function_msg").innerHTML = "";
    document.getElementById("functions").innerHTML = "";

}

function saveResults() {
    var functions = document.getElementById("functions");
    var select_functions = functions.querySelectorAll('input[type=checkbox]');
    var saved_functions = document.getElementById("saved_functions");
    if (!saved_functions.innerHTML) {
        saved_functions.innerHTML = "<h2>Saved Functions</h2>";
    }

    select_functions.forEach(function (el) {
        if (el.checked) {
            if(saved_functions.querySelectorAll("div[data-uuid=\"" + el.parentElement.getAttribute("data-uuid") + "\"]").length===0) {
                saved_functions.appendChild(el.parentElement.parentElement.cloneNode(true));
            }
        }
    });
}

function toJsonAndPush() {
    //    extract all names
    console.log("Called");
    const form = document.getElementById("add_function_form");
    const formData = new FormData(form);
    var dict = { generics: [] };
    for (var pair of formData.entries()) {
        if (pair[0] === "inputs" || pair[0] === "outputs") {
            console.log("Inputs0", pair[1].split(","))
            dict[pair[0]] = pair[1].split(",");
        } else {
            dict[pair[0]] = pair[1];
        }
    }

    const xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "/func",true);
    xmlhttp.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
    console.log(JSON.stringify(dict));
    xmlhttp.send(JSON.stringify(dict));

    form.reset();
}