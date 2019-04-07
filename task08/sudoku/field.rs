/**
 * В этом модуле лежат всякие вспомогательные определения
 * для нашего main
 */
use std::fmt;

// Размеры поля
pub const K: usize = 3;
pub const N: usize = K * K;

// Структрка для клетки
// Клетка либо пуста, либо в ней написано беззнаковое целое число
// Мы просим ее реализовать интерфейсы:
// Copy -- для копирования клетки(с помощью оператора = например)
// Clone -- для клонирования(.clone())
// PartialEq -- для проверки на равенство(подробнее?)
#[derive(Copy, Clone, PartialEq)]
pub enum Cell {
    Empty,
    Digit(usize),
}

// Что бы постоянно не писать Cell::Empty "раскроем" внутренности Cell
use Cell::*;

// напишем свой отладочный принтер для клетк
impl fmt::Debug for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Empty => '.',
                Digit(x) => ('0' as usize + x) as u8 as char,
            }
        )
    }
}

// Аналогично для поля -- массива из N массивов из N клеток
#[derive(Clone)]
pub struct Field(pub [[Cell; N]; N]);

// У поля есть некоторые методы, которыми мы пользуемся в main
// Это их реализации
// pub -- это модификатор доступа -- все в этом файле торчит наружу
impl Field {
    // Создание пустого поля
    pub fn empty() -> Field {
        Field([[Empty; N]; N])
    }

    // Проверка, что поле заполнено
    pub fn full(&self) -> bool {
        self.0.iter().all(|cells| cells.iter().all(|c| *c != Empty))
    }

    // Долгая проверка на то, что поле противоречиво
    pub fn contradictory(&self) -> bool {
        // Посмотрели на все строки
        for row in 0..N {
            let mut was = [false; N + 1];
            for col in 0..N {
                if let Digit(val) = self.0[row][col] {
                    let val = val as usize;
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // На все столбцы
        for col in 0..N {
            let mut was = [false; N + 1];
            for row in 0..N {
                if let Digit(val) = self.0[row][col] {
                    let val = val as usize;
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // И на диагонали
        for row_0 in 0..K {
            for col_0 in 0..K {
                let mut was = [false; N + 1];
                for row_d in 0..K {
                    for col_d in 0..K {
                        if let Digit(val) = self.0[row_0 * K + row_d][col_0 * K + col_d] {
                            let val = val as usize;
                            if was[val] {
                                return true;
                            }
                            was[val] = true;
                        }
                    }
                }
            }
        }
        false
    }
}

// Отладочный вывод, но уже для поля
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (row_id, cells) in self.0.iter().enumerate() {
            for cell in cells.iter() {
                write!(f, "{:?}", cell)?;
            }
            if row_id + 1 < N {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

// Эта функция пытается распарсить данное ей строковое представление поля
// в нашу структуру. Если у нее не получается -- падает с ошибкой
pub fn parse_field<I>(reader: I) -> Field
where
    I: IntoIterator<Item = String>,
{
    let mut result = Field::empty();
    let mut reader = reader.into_iter();
    // Читаем N строчек
    for row in 0..N {
        let line = reader.next().expect("Not enough lines provided");
        assert_eq!(line.len(), N);

        for (col, ch) in line.chars().enumerate() {
            result.0[row][col] = match ch {
                '.' => Empty,
                '1'..='9' => Digit(ch.to_digit(10).unwrap() as usize),
                _ => panic!(format!("Unknown character: {}", ch)),
            }
        }
    }
    assert_eq!(reader.next(), None);
    result
}
