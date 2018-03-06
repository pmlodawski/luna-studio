
lunaPrefix = 'luna-'

export luna = (list) =>
    prefixed = []
    for item in list
        prefixed.push(lunaPrefix + item)
    prefixed.join ' '

