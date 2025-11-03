/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

// check search term
const source = document.getElementById('search-text');



function fmtLiteral(val) {
    let output = document.createElement('span');
    if (val.hasOwnProperty('U8')) {
        output.appendChild(document.createTextNode(val['U8']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('U16')) {
        output.appendChild(document.createTextNode(val['U16']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('U32')) {
        output.appendChild(document.createTextNode(val['U32']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('U64')) {
        output.appendChild(document.createTextNode(val['U64']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('U128')) {
        output.appendChild(document.createTextNode(val['U128']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('I8')) {
        output.appendChild(document.createTextNode(val['I8']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('I16')) {
        output.appendChild(document.createTextNode(val['I16']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('I32')) {
        output.appendChild(document.createTextNode(val['I32']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('I64')) {
        output.appendChild(document.createTextNode(val['I64']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('I128')) {
        output.appendChild(document.createTextNode(val['I128']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('F32')) {
        output.appendChild(document.createTextNode(val['F32']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('F64')) {
        output.appendChild(document.createTextNode(val['F64']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('Str')) {
        output.appendChild(document.createTextNode(`"${val['Str']}"`));
        output.setAttribute('class', 'str-literal');
    } else if (val.hasOwnProperty('Char')) {
        output.appendChild(document.createTextNode(`'${val['Char']}'`));
        output.setAttribute('class', 'char-literal');
    } else if (val.hasOwnProperty('Bool')) {
        output.appendChild(document.createTextNode(val['Bool']));
        output.setAttribute('class', 'bool-literal');
    } else if (val.hasOwnProperty('Usize')) {
        output.appendChild(document.createTextNode(val['Usize']));
        output.setAttribute('class', 'num-literal');
    } else if (val.hasOwnProperty('Isize')) {
        output.appendChild(document.createTextNode(val['Isize']));
        output.setAttribute('class', 'num-literal');
    } else {
        output.appendChild(document.createTextNode(JSON.stringify(val)));
    }
    return output;
}

function literalAsString(val) {
    if (val.hasOwnProperty('U8')) {
        return val['U8'];
    } else if (val.hasOwnProperty('U16')) {
        return val['U16'];
    } else if (val.hasOwnProperty('U32')) {
        return val['U32'];
    } else if (val.hasOwnProperty('U64')) {
        return val['U64'];
    } else if (val.hasOwnProperty('U128')) {
        return val['U128'];
    } else if (val.hasOwnProperty('I8')) {
        return val['I8'];
    } else if (val.hasOwnProperty('I16')) {
        return val['I16'];
    } else if (val.hasOwnProperty('I32')) {
        return val['I32'];
    } else if (val.hasOwnProperty('I64')) {
        return val['I64'];
    } else if (val.hasOwnProperty('I128')) {
        return val['U128'];
    } else if (val.hasOwnProperty('F32')) {
        return val['F32'];
    } else if (val.hasOwnProperty('F64')) {
        return val['F64'];
    } else if (val.hasOwnProperty('Str')) {
        return val['Str'];
    } else if (val.hasOwnProperty('Char')) {
        return val['Char'];
    } else if (val.hasOwnProperty('Bool')) {
        return val['Bool'];
    } else if (val.hasOwnProperty('Usize')) {
        return val['Usize'];
    } else if (val.hasOwnProperty('Isize')) {
        return val['Isize'];
    } else {
        output.appendChild(document.createTextNode(JSON.stringify(val)));
    }
}

function fmtQualifierName(qualifier_name, tail_class) {
    let name = document.createElement('span')
    for (let i = 0; i < qualifier_name.path.length; i++) {
        let el = document.createElement('span');
        el.appendChild(document.createTextNode(qualifier_name.path[i]));
        if (i + 1 === qualifier_name.path.length) {
            el.setAttribute('class', tail_class)
        } else {
            el.setAttribute('class', 'path-segment')
        }
        name.appendChild(el);
        // insert `::` as delimiters between name segments
        if (i + 1 < qualifier_name.path.length) {
            name.appendChild(punct('::'));
        }
    }
    return name;
}

function qualifierAsString(qualifierName) {
    let name = "";
    for (let i = 0; i < qualifierName.path.length; i++) {
        name += qualifierName.path[i];
        if (i + 1 < qualifierName.path.length) {
            name += "::";
        }
    }
    return name;
}

function fmtDocConstValue(env) {
    let output = document.createElement('span');
    if (env.hasOwnProperty('Const')) {
        // format as constant
        output.appendChild(fmtTypeName(env['Const'], 'const-name'));
    } else if (env.hasOwnProperty('Literal')) {
        // format as literal
        output.appendChild(fmtLiteral(env['Literal']));
    } else if (env.hasOwnProperty('Elicit')) {
        // format elicit
        output.appendChild(punct('_'));
    }
    output.setAttribute('class', 'const-name');
    return output;
}

function docConstValueAsString(env) {
    if (env.hasOwnProperty('Const')) {
        return typeNameAsString(env['Const'])
    } else if (env.hasOwnProperty('Literal')) {
        return literalAsString(env['Literal'])
    } else if (env.hasOwnProperty('Elicit')) {
        return '_';
    }
    return undefined;
}

function fmtEnvInstDoc(env) {
    let output = document.createElement('span');
    output.appendChild(punct('<'));
    for (let i = 0; i < env.params.length; i++) {
        const item = env.params[i];
        if (item.hasOwnProperty('Type')) {
            output.appendChild(fmtTypeDoc(item['Type']['ty']));
        } else if (item.hasOwnProperty('Const')) {
            output.appendChild(fmtDocConstValue(item['Const']['val']));
        } else if (item.hasOwnProperty('ElicitType')) {
            output.appendChild(type_name('_'));
        } else if (item.hasOwnProperty('ElicitConst')) {
            output.appendChild(const_name('_'));
        }
        // add delimiters
        if (i + 1 < env.params.length) {
            output.appendChild(punct(', '));
        }
    }
    output.appendChild(punct('>'));
    return output;
}

function fmtEnvDoc(env) {
    let output = document.createElement('span');
    output.appendChild(punct('<'));
    for (let i = 0; i < env.params.length; i++) {
        const param = env.params[i];
        if (param.hasOwnProperty('Type')) {
            output.appendChild(type_name(param['Type'].name));
        } else if (param.hasOwnProperty('Const')) {
            output.appendChild(const_name(param['Const'].name));
            output.appendChild(punct(': '));
            output.appendChild(fmtTypeDoc(param['Const'].ty));
        }

        if (i + 1 < env.params.length) {
            output.appendChild(punct(', '));
        }
    }
    output.appendChild(punct('>'));
    return output;
}

function fmtTypeNameSegments(name, tail_class = 'type-name', force_turbo_fish = false) {
    let output = document.createElement('span');
    output.appendChild(fmtQualifierName(name.name, tail_class));
    if (name.parameters.params.length > 0) {
        if (force_turbo_fish) {
            output.appendChild(punct('::'));
        }
        output.appendChild(fmtEnvInstDoc(name.parameters));
    }
    return output;
}

function fmtTypeName(name, tail_class='type-name', force_turbo_fish=false) {
    if (name.length > 1) {
        let output = document.createElement('span');
        for (let i = 0; i < name.length; i++) {
            output.appendChild(fmtTypeNameSegments(name[i], tail_class, true));
            if (i + 1 < name.length) {
                output.appendChild(punct('::'));
            }
        }
        return output;
    }
    return fmtTypeNameSegments(name[0], tail_class, force_turbo_fish)
}

function typeNameAsString(name) {
    if (name.length > 1) {
        let output = "";
        for (let i = 0; i < name.length; i++) {
            output += qualifierAsString(name[i].name);
            if (i + 1 < name.length) {
                output += '::';
            }
        }
        return output;
    }
    return qualifierAsString(name[0].name);
}

function typeAsString(val) {
    if (val.hasOwnProperty('Base')) {
        return typeNameAsString(val['Base'][0])
    } else if (val.hasOwnProperty('Array')) {
        let output = "";
        output += '[';
        output += typeAsString(val['Array'][0]);
        output += '; ';
        output += docConstValueAsString(val['Array'][1]);
        output += ']';
        return output;
    } else if (val.hasOwnProperty('Slice')) {
        let output = "";
        output += "[";
        output += typeAsString(val['Slice'][0]);
        output += "]";
        return output;
    } else if (val.hasOwnProperty('Ref')) {
        let output = "&";
        output += typeAsString(val['Ref'][0]);
        return output;
    } else if (val.hasOwnProperty('MutRef')) {
        let output = "&mut ";
        output += typeAsString(val['MutRef'][0]);
        return output;
    } else if (val.hasOwnProperty('Empty')) {
        return "()";
    } else if (val.hasOwnProperty('Tuple')) {
        let output = "(";
        for (let i = 0; i < val['Tuple'][0].length; i++) {
            output += typeAsString(val['Tuple'][0][i]);
            if (i + 1 < val['Tuple'][0].length) {
                output += ", ";
            }
        }
        output += ")";
        return output;
    } else if (val.hasOwnProperty('Elicit')) {
        return "_";
    }
}

function fmtTypeDoc(val, force_turbo_fish=false) {
    if (val.hasOwnProperty('Base')) {
        let el = fmtTypeName(val['Base'][0], "type-name", force_turbo_fish);
        el.setAttribute('class', 'reference-item');

        const handleTypeClick = function (e) {
            source.value = typeAsString(val);
            let event = new KeyboardEvent("keypress", {'key': 'Enter'});
            source.dispatchEvent(event);
            return false;
        }
        el.addEventListener('click', handleTypeClick);
        return el;
    } else if (val.hasOwnProperty('Array')) {
        let output = document.createElement('span');
        output.appendChild(punct('['));
        output.appendChild(fmtTypeDoc(val['Array'][0]));
        output.appendChild(punct('; '))

        let len = document.createElement('span');
        len.appendChild(fmtDocConstValue(val['Array'][1]));
        output.appendChild(len);
        output.appendChild(punct(']'));
        return output;
    } else if (val.hasOwnProperty('Slice')) {
        let output = document.createElement('span');
        output.appendChild(punct('['));
        output.appendChild(fmtTypeDoc(val['Slice'][0]));
        output.appendChild(punct(']'));
        return output;
    } else if (val.hasOwnProperty('Ref')) {
        let output = document.createElement('span');
        output.appendChild(punct('&'));
        output.appendChild(fmtTypeDoc(val['Ref'][0]));
        return output;
    } else if (val.hasOwnProperty('MutRef')) {
        let output = document.createElement('span');
        output.appendChild(punct('&mut '));
        output.appendChild(fmtTypeDoc(val['MutRef'][0]));
        return output;
    } else if (val.hasOwnProperty('Empty')) {
        return punct('()');
    } else if (val.hasOwnProperty('Tuple')) {
        let output = document.createElement('span');
        output.appendChild(punct('('));
        for (let i = 0; i < val['Tuple'][0].length; i++) {
            output.appendChild(fmtTypeDoc(val['Tuple'][0][i]));
            if (i + 1 < val['Tuple'][0].length) {
                output.appendChild(punct(', '));
            }
        }
        output.appendChild(punct(')'));
        return output;
    } else if (val.hasOwnProperty('Elicit')) {
        return punct('_');
    }
}

function fmtModifier(val) {
    if (val === 'Comptime') {
        return keyword('comptime');
    } else if (val === 'MaybeComptime') {
        let output = document.createElement('span');
        output.appendChild(operator('?'));
        output.appendChild(keyword('comptime'));
        return output;
    } else if (val === 'Mut') {
        return keyword('mut');
    } else if (val === 'Async') {
        return keyword('async');
    } else {
        keyword('error')
    }
}

function fmtModifiers(val) {
    let output = document.createElement('span');
    for (let i = 0; i < val.length; i++) {
        output.appendChild(fmtModifier(val[i]));
        output.appendChild(punct(' '));
    }
    return output;
}

function punct(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'decorator');
    output.appendChild(document.createTextNode(s));
    return output;
}

function operator(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'operator');
    output.appendChild(document.createTextNode(s));
    return output;
}

function keyword(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'keyword');
    output.appendChild(document.createTextNode(s));
    return output;
}

function type_name(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'type-name');
    output.appendChild(document.createTextNode(s));
    return output;
}

function const_name(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'const-name');
    output.appendChild(document.createTextNode(s));
    return output;
}

function var_name(s) {
    let output = document.createElement('span');
    output.setAttribute('class', 'var-name');
    output.appendChild(document.createTextNode(s));
    return output;
}

function fmtMarkdown(text) {
    let doc_div = document.createElement('div');
    const ps = text.split("\n\n");
    for (let j = 0; j < ps.length; j++) {
        let p = document.createElement('p');
        p.appendChild(document.createTextNode(ps[j]));
        doc_div.appendChild(p);
    }
    doc_div.setAttribute('class', 'doc-text')
    return doc_div;
}

/**
 * Uses canvas.measureText to compute and return the width of the given text of given font in pixels.
 *
 * @param {String} text The text to be rendered.
 * @param {String} font The css font descriptor that text is to be rendered with (e.g. "bold 14px verdana").
 *
 * @see https://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
 */
function getTextWidth(text, font) {
    // re-use canvas object for better performance
    const canvas = getTextWidth.canvas || (getTextWidth.canvas = document.createElement("canvas"));
    const context = canvas.getContext("2d");
    context.font = font;
    return context.measureText(text);
}

function getCssStyle(element, prop) {
    return window.getComputedStyle(element, null).getPropertyValue(prop);
}

function getCanvasFont(el = document.body) {
    const fontWeight = getCssStyle(el, 'font-weight') || 'normal';
    const fontSize = getCssStyle(el, 'font-size') || '1em';
    const fontFamily = getCssStyle(el, 'font-family') || 'system-ui';

    return `${fontWeight} ${fontSize} ${fontFamily}`;
}

function populateContent(filter) {
    console.log("creating documentation for items");
    console.log(filter);
    let content_div = document.getElementById('doc-content');
    content_div.innerHTML = ""; // clear all content that has so far been part of the content map

    // create functions
    let functions = document.createElement('ul');
    for (let i = 0; i < docs['fns'].length; i++) {
        const fn = docs['fns'][i];
        // filter
        let fullFuncName = "";
        if (fn.associated_type != null) {
            fullFuncName += typeAsString(fn.associated_type);
            fullFuncName += "::";
        }
        fullFuncName += qualifierAsString(fn.name);

        const index = fullFuncName.toLowerCase().indexOf(filter);
        if (filter.length !== 0 && index === -1) {
            continue;
        }

        // format string into readable HTML
        let func_doc = document.createElement('span');
        if (fn.ms.length > 0) {
            func_doc.appendChild(fmtModifiers(fn.ms));
        }

        // create function keyword
        func_doc.appendChild(keyword('fn'));
        func_doc.appendChild(punct(' '));

        let func_name = document.createElement('span');
        if (fn.associated_type != null) {
            func_name.appendChild(fmtTypeDoc(fn.associated_type, true));
            func_name.appendChild(punct('::'));
        }
        func_name.appendChild(fmtQualifierName(fn.name, 'fn-name'));

        // insert marker to mark name
        if (index !== -1) {
            let leading = getTextWidth(fullFuncName.substring(0, index), `1.0em system-ui`).width;
            let size = getTextWidth(filter, `1.0em system-ui`);
            console.log(leading);

            let marker = document.createElement('span');
            marker.setAttribute('class', 'marker');
            marker.setAttribute('style', `margin-left: ${leading*1.6}px; width: ${size.width*1.6}px; height: 1.5em;`);
            func_doc.appendChild(marker);
        }
        func_doc.appendChild(func_name);

        // format parameter env
        if (fn.env.params.length > 0) {
            func_doc.appendChild(fmtEnvDoc(fn.env));
        }

        func_doc.appendChild(punct('('));
        if (fn.params.length > 0) {
            func_doc.appendChild(document.createElement('br'));
        }
        // format parameters
        for (let j = 0; j < fn.params.length; j++) {
            const param = fn.params[j];

            let tab = document.createElement('span');
            tab.appendChild(document.createTextNode("    "));
            tab.setAttribute('style', 'display: inline-block; width: 50px;');
            func_doc.appendChild(tab);

            if (param.ms.length > 0) {
                func_doc.appendChild(fmtModifiers(param.ms));
            }

            func_doc.appendChild(var_name(param.name));
            func_doc.appendChild(punct(': '));
            func_doc.appendChild(fmtTypeDoc(param.ty));

            if (j + 1 < fn.params.length) {
                func_doc.appendChild(punct(', '));
            }
            func_doc.appendChild(document.createElement('br'));
        }

        func_doc.appendChild(punct(')'));
        // format return value
        if (fn.ret !== 'Empty') {
            func_doc.appendChild(punct(' -> '));
            func_doc.appendChild(fmtTypeDoc(fn.ret));
        }

        let code_container = document.createElement('div');
        code_container.setAttribute('class', 'code');
        code_container.appendChild(func_doc);

        // create wrapper with documentation
        let doc_wrapper = document.createElement('li');
        doc_wrapper.appendChild(fmtMarkdown(fn.doc));
        doc_wrapper.appendChild(code_container);
        functions.appendChild(doc_wrapper);
    }

    let func_box = document.createElement('div');
    let funch = document.createElement('h1');
    funch.appendChild(document.createTextNode('Functions'));
    func_box.appendChild(funch);
    func_box.appendChild(functions);
    content_div.appendChild(func_box);

    // append global variables
    let lets = document.createElement('ul');
    for (let i = 0; i < docs['lets'].length; i++) {
        const l = docs['lets'][i];
        // filter
        let fullFuncName = qualifierAsString(l.name);
        const index = fullFuncName.toLowerCase().indexOf(filter);
        if (filter.length !== 0 && index === -1) {
            continue;
        }


        let variable = document.createElement('span');
        variable.appendChild(keyword('let'));
        variable.appendChild(punct(' '));
        if (l.ms.length > 0) {
            variable.appendChild(fmtModifiers(l.ms));
        }

        // insert marker to mark name
        if (index !== -1) {
            let leading = getTextWidth(fullFuncName.substring(0, index), `1.0em system-ui`).width;
            let size = getTextWidth(filter, `1.0em system-ui`);
            console.log(leading);

            let marker = document.createElement('span');
            marker.setAttribute('class', 'marker');
            marker.setAttribute('style', `margin-left: ${leading*1.6}px; width: ${size.width*1.6}px; height: 1.5em;`);
            variable.appendChild(marker);
        }
        // create name and type identifier
        variable.appendChild(fmtQualifierName(l.name, 'var-name'));

        variable.appendChild(punct(': '));
        variable.appendChild(fmtTypeDoc(l.ty));
        variable.appendChild(punct(' = '));
        variable.appendChild(operator('..'));

        variable.setAttribute('class', 'code');

        // create wrapper with documentation
        let doc_wrapper = document.createElement('li');
        doc_wrapper.appendChild(fmtMarkdown(l.doc));
        doc_wrapper.appendChild(variable);
        lets.appendChild(doc_wrapper);
    }

    let let_box = document.createElement('div');
    let leth = document.createElement('h1');
    leth.appendChild(document.createTextNode('Global Variables'));
    let_box.appendChild(leth);
    let_box.appendChild(lets);
    content_div.appendChild(let_box);

    // append global constants
    let consts = document.createElement('ul');
    for (let i = 0; i < docs['consts'].length; i++) {
        const c = docs['consts'][i];
        // filter
        let fullFuncName = "";
        if (c.associated_type != null) {
            fullFuncName += typeAsString(c.associated_type);
            fullFuncName += "::";
        }
        fullFuncName += qualifierAsString(c.name);

        const index = fullFuncName.toLowerCase().indexOf(filter);
        if (filter.length !== 0 && index === -1) {
            continue;
        }


        let constant = document.createElement('span');
        constant.appendChild(keyword('const'));
        constant.appendChild(punct(' '));
        if (c.ms.length > 0) {
            constant.appendChild(fmtModifiers(c.ms));
        }
        // create name and type identifier
        let const_name = document.createElement('span');
        if (c.associated_type != null) {
            const_name.appendChild(fmtTypeDoc(c.associated_type));
            const_name.appendChild(punct('::'));
        }
        const_name.appendChild(fmtQualifierName(c.name, 'const-name'));

        // insert marker to mark name
        if (index !== -1) {
            let leading = getTextWidth(fullFuncName.substring(0, index), `1.0em system-ui`).width;
            let size = getTextWidth(filter, `1.0em system-ui`);
            console.log(leading);

            let marker = document.createElement('span');
            marker.setAttribute('class', 'marker');
            marker.setAttribute('style', `margin-left: ${leading*1.6}px; width: ${size.width*1.6}px; height: 1.5em;`);
            constant.appendChild(marker);
        }
        constant.appendChild(const_name);

        // add type identifier
        constant.appendChild(punct(': '));
        constant.appendChild(fmtTypeDoc(c.ty));
        constant.appendChild(punct(' = '));
        constant.appendChild(operator('..'));

        constant.setAttribute('class', 'code');

        // create wrapper with documentation
        let doc_wrapper = document.createElement('li');
        doc_wrapper.appendChild(fmtMarkdown(c.doc));
        doc_wrapper.appendChild(constant);
        consts.appendChild(doc_wrapper);
    }

    let const_box = document.createElement('div');
    let consth = document.createElement('h1');
    consth.appendChild(document.createTextNode('Global Constants'));
    const_box.appendChild(consth);
    const_box.appendChild(consts);
    content_div.appendChild(const_box);

    // append modules
    let modules = document.createElement('ul');
    for (let i = 0; i < docs['modules'].length; i++) {
        const m = docs['modules'][i];
        // filter
        let fullFuncName = qualifierAsString(m.name);
        const index = fullFuncName.toLowerCase().indexOf(filter);
        if (filter.length !== 0 && index === -1) {
            continue;
        }

        let module = document.createElement('span');
        module.appendChild(keyword('mod'));
        module.appendChild(punct(' '));

        // add marker
        if (index !== -1) {
            let leading = getTextWidth(fullFuncName.substring(0, index), `1.0em system-ui`).width;
            let size = getTextWidth(filter, `1.0em system-ui`);
            console.log(leading);

            let marker = document.createElement('span');
            marker.setAttribute('class', 'marker');
            marker.setAttribute('style', `margin-left: ${leading*1.6}px; width: ${size.width*1.6}px; height: 1.5em;`);
            module.appendChild(marker);
        }

        module.appendChild(fmtQualifierName(m.name, 'path-segment'));
        module.setAttribute('class', 'code');

        // create wrapper with documentation
        let doc_wrapper = document.createElement('li');
        doc_wrapper.appendChild(fmtMarkdown(m.doc));
        doc_wrapper.appendChild(module);
        modules.appendChild(doc_wrapper);
    }

    let module_box = document.createElement('div');
    let moduleh = document.createElement('h1');
    moduleh.appendChild(document.createTextNode('Modules'));
    module_box.appendChild(moduleh);
    module_box.appendChild(modules);
    content_div.appendChild(module_box);
}


const inputHandler = function(e) {
    if (e.key === 'Enter') {
        populateContent(source.value.toLowerCase());
    }
}

source.addEventListener('keypress', inputHandler);
populateContent('');


