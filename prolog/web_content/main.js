function searchAndDisplay() {
    const xmlhttp = new XMLHttpRequest();
    const form = document
        .getElementById("search_form");
    const url = form.action + "?" + serialize(form);
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
            output += "<h3>" + json["functions"][i]["name"] + "</h3>"
            output += "<p>" + json["functions"][i]["docs"] + "</p>"
        }
        document.getElementById("functions").innerHTML = output;
    }
}
