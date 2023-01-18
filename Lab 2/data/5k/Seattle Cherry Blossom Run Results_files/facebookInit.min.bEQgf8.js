var FbInitialized=!1,facebookLoadCallbacks=[];
function doAsyncInit(b){var c=window.fbAsyncInit?window.fbAsyncInit:null;window.fbAsyncInit=function(){null!==c&&c();FB.init({appId:b,cookie:!0,version:"v6.0"});FbInitialized=!0;for(var a=0;a<facebookLoadCallbacks.length;a++)facebookLoadCallbacks[a].call(null)};(function(a,b,c){var d=a.getElementsByTagName(b)[0];a.getElementById(c)||(a=a.createElement(b),a.id=c,a.src="https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v6.0",d.parentNode.insertBefore(a,d))})(document,"script","facebook-jssdk")}
function whenFbLoaded(b){FbInitialized?b():facebookLoadCallbacks.push(b)};

//f226106f378a2e51187d874126ef0eaf
