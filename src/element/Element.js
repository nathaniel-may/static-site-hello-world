'use strict;'

export const replaceChildren = function (elem) {
    return function (elems) {
        elem.replaceChildren(...elems);
    };
}