function rember(a, lat) {
    if (lat.length === 0) {
        return []
    } else {
        const first = lat.shift()
        if (first === a) {
            return lat
        } else {
            const rest = rember(a, lat)
            rest.unshift(first)
            return rest
        }
    }
}
console.log(rember(1, [12, 4, 1, 83274898347, 3]))