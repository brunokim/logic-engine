
export class Wam2 {
    constructor(states, clauses) {
        this.states = states;
        this.clauses = clauses;

        this.time = 0;

        this.keyListener = this.handleKeypress.bind(this)
        window.addEventListener("keydown", this.keyListener)
    }
    
    destroy() {
        window.removeEventListener("keydown", this.keyListener)
    }

    handleKeypress(evt) {
        if (evt.defaultPrevented) {
            return
        }

        let handled = false
        switch(evt.key) {
        case "ArrowLeft":
            this.addTime(-1)
            handled = true
            break
        case "ArrowRight":
            this.addTime(+1)
            handled = true
            break
        }

        if (handled) {
            evt.preventDefault()
        }
    }

    render() {
        $("#wam2")
            .html("")
            .append(this.controls())
            .append($("<div>").append(this.globalsTable()))
            .append($("<div>").append(this.tempVarsTable(this.tempVars())))
            .append(this.envStack())
            .append(this.choiceStack())
    }

    controls() {
        return $("<div>")
            .addClass("controls")
            .append($("<button>").text("-100").click(() => this.addTime(-100)))
            .append($("<button>").text("-10").click(() => this.addTime(-10)))
            .append($("<button>").text("-1").click(() => this.addTime(-1)))
            .append($("<input>")
                .attr("type", "number")
                .val(this.time)
                .change((evt) => this.goToTime(evt.target.valueAsNumber)))
            .append($("<button>").text("+1").click(() => this.addTime(+1)))
            .append($("<button>").text("+10").click(() => this.addTime(+10)))
            .append($("<button>").text("+100").click(() => this.addTime(+100)))
    }

    addTime(dt) {
        this.goToTime(this.time+dt)
    }

    goToTime(t) {
        if (t < 0) {
            t = 0
        }
        if (t > this.states.length-1) {
            t = this.states.length-1
        }
        if (t != this.time) {
            this.time = t
            this.render()
        }
    }

    state(time) {
        if (time === undefined) {
            time = this.time
        }
        return this.states[time]
    }

    clause(ptr = this.state().CodePtr) {
        return this.clauses[ptr.ClausePos]
    }

    env(state = this.state(), pos = state.EnvPos) {
        return pos === null ? null : state.Envs[pos]
    }

    choice(state = this.state(), pos = state.ChoicePos) {
        return pos === null ? null : state.ChoicePoints[pos]
    }

    globalsTable(state = this.state()) {
        return $("<table>")
            .addClass("globals")
            .append($("<tbody>")
                .append($("<tr>")
                    .append($("<td>").text("Continuation"))
                    .append($("<td>").text(this.instructionAddress(state.Continuation)))))
    }

    tempVars(state = this.state()) {
        let clause = this.clause(state.CodePtr)
        let numRegisters = clause.NumRegisters
        return state.Reg.slice(0, numRegisters)
    }

    // Register ("temporary") variables
    tempVarsTable(regs) {
        let tbody = $("<tbody>");
        for (let i = 0; i < regs.length; i++) {
            tbody.append($("<tr>")
                .append($("<td>").text(`X${i}`))
                .append($("<td>").text(regs[i])))
        }

        return $("<table>")
            .addClass("registers")
            .append($("<thead>")
                .append(`<th colspan="2">Register bank</th>`))
            .append(tbody);
    }

    envStack(state = this.state()) {
        let pos = state.EnvPos;
        let ptr = state.CodePtr;
        let i = 0;

        let ptrStyle = state.Backtrack ? "backtrack" : "active";
        if (pos === null) {
            return $("<div>").append(this.instructionTable(ptr, ptrStyle))
        }

        let stack = $("<div>")
        while (pos !== null) {
            let env = state.Envs[pos]

            let tableId = `env-${i}`;
            let isExpanded = (i == 0);
            let header = $(`
                <h3
                    role="button"
                    aria-haspopup=true
                    aria-controls=${tableId}
                    aria-expanded=${isExpanded}>
                        #${i} --
                        Address: ${this.instructionAddress(ptr)}
                        Continuation: ${this.instructionAddress(env.Continuation)}
                </h3>`)
                .addClass("env-header")
                .click(() => {
                    isExpanded = !isExpanded
                    envTable.toggleClass("hidden")
                    header.attr("aria-expanded", isExpanded)
                })

            if (i > 0) {
                ptrStyle = "active"
            }
            let envTable = this.envTable(state, pos, ptr, ptrStyle).attr("id", tableId);
            if (!isExpanded) {
                envTable.addClass("hidden")
            }
            stack.append($("<div>")
                .append(header)
                .append(envTable))

            pos = env.PrevPos;
            ptr = env.Continuation;
            i++;
        }
        return stack;
    }

    choiceStack(state = this.state()) {
        let pos = state.ChoicePos;
        let i = 0;
        let trailSize = 10000;

        let stack = $("<div>")
        while (pos !== null) {
            let choice = state.ChoicePoints[pos]
            let ptr = choice.NextAlternative
            let boundRefs = state.Trail.slice(choice.TrailSize, trailSize);
            trailSize = choice.TrailSize;

            let tableId = `choice-${i}`;
            let choiceTable = this.choiceTable(state, pos, boundRefs)
                .attr("id", tableId)
                .addClass("hidden");
            let isExpanded = false
            let header = $(`
                <h3
                    role="button"
                    aria-haspopup=true
                    aria-controls=${tableId}
                    aria-expanded=${isExpanded}>
                        #${i} --
                        Address: ${this.instructionAddress(ptr)}
                        Continuation: ${this.instructionAddress(choice.Continuation)}
                </h3>`)
                .addClass("choice-header")
                .click(() => {
                    isExpanded = !isExpanded
                    choiceTable.toggleClass("hidden")
                    header.attr("aria-expanded", isExpanded)
                })
            stack.append($("<div>").append(header).append(choiceTable))
            pos = choice.PrevPos;
            i++;
        }
        return stack;
    }

    envTable(state = this.state(), pos = state.EnvPos, ptr = state.CodePtr, ptrStyle = "active") {
        return $("<div>")
            .addClass("container")
            .append(this.instructionTable(ptr, ptrStyle))
            .append(this.permVarsTable(this.env(state, pos)))
    }

    choiceTable(state = this.state(), pos = state.ChoicePos, refs) {
        let choice = this.choice(state, pos)
        if (choice === null) {
            return $("<div>")
        }
        return $("<div>")
            .addClass("container")
            .append(this.instructionTable(choice.NextAlternative))
            .append($("<div>")
                .append(this.tempVarsTable(choice.Args))
                .append(this.permVarsTable(this.env(state, choice.EnvPos)))
                .append(this.trailTable(refs)))
    }

    // Local ("permanent") variables
    permVarsTable(env = this.env()) {
        let tbody = $("<tbody>");
        if (env !== null) {
            let vars = env.PermanentVars
            for (let i = 0; i < vars.length; i++) {
                tbody.append($("<tr>")
                    .append($("<td>").text(`Y${i}`))
                    .append($("<td>").text(vars[i])))
            }
        }

        return $("<table>")
            .addClass("registers")
            .append($("<thead>")
                .append($(`<th colspan="2">Permanent vars</th>`)))
            .append(tbody);
    }

    trailTable(refs) {
        if (refs === undefined || refs.length == 0) {
            return null;
        }
        let tbody = $("<tbody>")
        for (let ref of refs) {
            tbody.append($("<tr>")
                .append($("<td>").text(`_X${ref.Id}`))
                .append($("<td>").text(ref.Term)))
        }
        return $("<table>")
            .addClass("trail")
            .append($("<thead>")
                .append($(`<th colspan="2">Trail</th>`)))
            .append(tbody);
    }

    instructionTable(ptr = this.state().CodePtr, ptrStyle = "active") {
        let clause = this.clause(ptr)
        let tbody = $("<tbody>")
        for (let i = 0; i < clause.Code.length; i++) {
            let instr = clause.Code[i]
            let row = this.instructionRow(instr)
            if (i == ptr.Pos) {
                row.addClass(ptrStyle);
            }
            tbody.append(row);
        }
        return $("<table>")
            .addClass("instructions")
            .append($("<thead>")
                .append(`<th colspan="3">${clause.Functor}#${ptr.ClausePos}</th>`))
            .append(tbody);
    }

    instructionAddress(ins) {
        if (ins === null) {
            return ""
        }
        let clause = this.clauses[ins.ClausePos]
        return `${clause.Functor}#${ins.ClausePos}[${ins.Pos}]`
    }

    instructionRow(instr) {
        let row = $("<tr>");
        row.append($("<td>").text(instructionTag(instr.Tag)))
        for (let arg of this.instructionArgs(instr)) {
            row.append($("<td>").append(arg))
        }
        return row;
    }

    instructionArgs(instr) {
        return [
            this.instructionFirstArg(instr),
            this.instructionSecondArg(instr),
        ]
    }

    instructionFirstArg(instr) {
        switch (instr.Tag) {
        case "PutStruct":
        case "GetStruct":
        case "Call":
        case "Execute":
            return instr.Functor
        case "PutConstant":
        case "GetConstant":
        case "SetConstant":
        case "UnifyConstant":
            return instr.Constant
        case "PutVariable":
        case "PutValue":
        case "GetVariable":
        case "GetValue":
        case "SetVariable":
        case "SetValue":
        case "UnifyVariable":
        case "UnifyValue":
        case "CallMeta":
        case "ExecuteMeta":
            return instr.Addr
        case "SetVoid":
        case "UnifyVoid":
        case "Allocate":
            return instr.NumVars
        case "PutList":
        case "GetList":
            return instr.ArgAddr
        case "TryMeElse":
        case "RetryMeElse":
            return this.instructionAddress(instr.Alternative)
        case "Try":
        case "Retry":
        case "Trust":
            return this.instructionAddress(instr.Continuation)
        case "SwitchOnTerm":
            return this.switchTable({
                'if_var': instr.IfVar,
                'if_const': instr.IfConstant,
                'if_list': instr.IfList,
                'if_struct': instr.IfStruct,
            })
        case "SwitchOnConstant":
        case "SwitchOnStruct":
            return this.switchTable(instr.Continuation)
        }
        return null
    }

    instructionSecondArg(instr) {
        switch (instr.Tag) {
        case "PutStruct":
        case "PutVariable":
        case "PutValue":
        case "PutConstant":
        case "GetStruct":
        case "GetVariable":
        case "GetValue":
        case "GetConstant":
            return instr.ArgAddr
        case "CallMeta":
        case "ExecuteMeta":
            return olist(instr.Params)
        }
        return null
    }

    switchTable(obj) {
        let table = $("<table>")
            .addClass("switch-table")
            .append($(`
                <thead>
                    <th>Key</th>
                    <th>Address</th>
                </thead>
            `))
            .append($("<tbody>"));
        for (let key of Object.keys(obj)) {
            let addr = this.instructionAddress(obj[key]);
            $("tbody", table).append($(`
                <tr>
                    <td>${key}</td>
                    <td>${addr}</td>
                </tr>
            `))
        }
        return table;
    }
}

// ----

function olist(items) {
    let list = $("<ol>")
    for (let item of items) {
        list.append($("<li>").text(item))
    }
    return list
}

function instructionTag(tag) {
    return toSnakeCase(fromCamelCase(tag))
}

function toSnakeCase(parts) {
    return parts.join("_")
}

function fromCamelCase(str) {
    return [...fromCamelCase_(str)]
}

function* fromCamelCase_(str) {
    let buf = ""
    for (let ch of str) {
        if (/[a-z]/.test(ch)) {
            buf += ch
            continue
        }
        if (buf != "") {
            yield buf
        }
        buf = ch.toLowerCase()
    }
    if (buf != "") {
        yield buf
    }
}
