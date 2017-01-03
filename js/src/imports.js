var sha3_256 = require('js-sha3').sha3_256;

var sha3s = {};
var sha3i = 1;

window['h$cryptonite_sha3_init'] = function(ctx, ctx_off) {
    ctx.u8[0] = sha3i++;
};

window['h$cryptonite_sha3_update'] = function(ctx, _, data, len) {
    var kekk = sha3s[ctx.u8[0]] || (sha3s[ctx.u8[0]] = sha3_256.create());
    kekk.update(data.u8);
};

window['h$cryptonite_sha3_finalize'] = function(ctx, _0, bits, out, _1) {
    // calling kekk.finalize here changes the hash wtf
    // and each time you call kekk.toString() it changes wth
    var kekk = sha3s[ctx.u8[0]];
    out.u8 = new Uint8Array(kekk.digest());
    delete sha3s[ctx.u8[0]];
    ctx.u8[0] = 0;
};

var sjcl = require('../bower_components/sjcl/sjcl.js');
var sha256 = sjcl.hash.sha256;



window['h$cryptonite_sha256_init'] = function(ctx, ctx_off) {
    //console.log('init', arguments);
    ctx.u8[0] = sha3i++;
};

window['h$cryptonite_sha256_update'] = function(ctx, _, data, len) {
    //console.log('update', data.i3);
    var hash = sha3s[ctx.u8[0]] || (sha3s[ctx.u8[0]] = new sha256());
    hash.update(data.i3);
};

window['h$cryptonite_sha256_finalize'] = function(ctx, _0, out, _1) {
    //console.log('finalize', arguments);
    var hash = sha3s[ctx.u8[0]];
    var data = hash.finalize();
    //out.i3 = new Int32Array(data).buffer;
    out.u8 = new Uint8Array(new Int32Array(data).buffer);
    console.log(out);
    delete sha3s[ctx.u8[0]];
    ctx.u8[0] = 0;
};
