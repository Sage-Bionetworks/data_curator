// // read the user login token from a cookie
// Shiny.addCustomMessageHandler("readCookie", function(message) {
//     var cookie = readCookie();
//     Shiny.onInputChange("cookie",cookie);
// });

// function readCookie() {
// 	name = "org.sagebionetworks.security.user.login.token";
// 	var nameEQ = name + "=";
// 	var ca = document.cookie.split(';');
// 	for(var i=0;i < ca.length;i++) {
// 		var c = ca[i];
// 		while (c.charAt(0)==' ') c = c.substring(1, c.length);
// 		if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length, c.length);
// 	}
// 	return null;
// }

// read the user login token from a cookie (new protocol) 
Shiny.addCustomMessageHandler("readCookie", function(message) {
    readCookie();
});

function readCookie() {
  const xhr = new XMLHttpRequest();
  const url='https://www.synapse.org/Portal/sessioncookie';
  xhr.withCredentials = true;
  xhr.onreadystatechange = function() {
     if (xhr.readyState == XMLHttpRequest.DONE) {
         Shiny.onInputChange("cookie",xhr.responseText);
     }
  }
  xhr.open("GET", url);
  xhr.send();
}
