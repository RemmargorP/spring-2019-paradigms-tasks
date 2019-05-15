-- Выведите название страны с максимальным уровнем грамотности по последним
-- данным, которые доступны для страны. В выводе: название страны и уровень
-- грамотности, именно в таком порядке и без лишних полей. (0,75 баллов)
SELECT Name FROM LiteracyRate JOIN Country ON CountryCode=Code GROUP BY Code HAVING MAX(Year) ORDER BY Rate DESC LIMIT 1;
