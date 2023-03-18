#pragma once

#include <string>
#include <map>

void error(const std::string& message);

template <typename K, typename V>
V* get_or_null(const std::map<K, V>& map, const K& key) {
    typename std::map<K, V>::const_iterator it = map.find(key);
    if (it == map.end())
        return nullptr;
    else
        return (V*)&it->second;
}