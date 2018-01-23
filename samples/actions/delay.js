/**
 *  testing for timeout
 */
function main(params) {
    let name = params.name || 'World';
    let delay = params.sleep * 1000 || 0;
    console.log(delay)
    return new Promise((resolve) => {
        setTimeout(() => {
            resolve({payload: 'Hello, ' + name + '!',
                    sleep: delay});
        }, delay);
    })
}

//main({"name":"whisk", "sleep":5}).then(a => console.log(a))
