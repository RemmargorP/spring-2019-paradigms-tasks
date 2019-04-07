//! Это основной файл с домашним заданием по языку Rust (task08).
//! Кроме него есть еще модуль `field.rs`, лежащий в этой же папке.
//! Полезные команды Cargo:
//! * `cargo build` — полная сборка программы (результат появляется в папке `target/debug`)
//! * `cargo build --release` — полная сборка программы с включёнными оптимизациями  (результат появляется в папке `target/release`)
//! * `cargo check` — проверка корректности программы без создания выполняемого файла.
//!       Отличается от `cargo build` тем, что работает сильно быстрее.
//! * `cargo clean` — удаление временных файлов
//!
//! Чтобы вам было легче ориентироваться, мы постарались прокомментировать весь код, насколько это возможно.
//! Если у вас остаются вопросы или непонятные места — пишите нам, поможем разобраться.

// Намек компилятору, что мы также хотим использовать наш модуль из файла `field.rs`.
mod field;

// Чтобы не писать `field::Cell:Empty`, можно "заимпортировать" нужные вещи из модуля.
use field::Cell::*;
use field::{parse_field, Field, N};

/**
 * Эта функция пытается "расширить" данное ей игровое поле
 * Под расширением мы понимаем просто добавление числа на поле
 * и попытку найти решение. Она принимает на вход три параметра:
 * 1. f: &mut Field -- мутабельную ссылку на поле(поле приходится менять и передавать туда-сюда)
 * 2. solved_cb : impl Fn(&mut Field) -> T -- замыкание, которые вызывается если мы нашли решение
 * 3. next_step_cb: impl Fn(&mut Field) -> Option<T> -- замыкание, которые вызывается, когда мы не
 * до конца заполнили поле, и над ним нужно сделать следующий шаг
 *
 * Замыкание -- это просто анонимная функция, котрая может что-либо захватить
 * (переменные из объемлющей области видимости). Здесь мы просим, что бы они захватывали
 * все по ссылке(Fn)
 *
 * В качестве результата она возвращает Option<T> -- это тип, который говорит нам о том, что
 * вычисление могло завершиться неудачей и результат не известен. По такому поводу у него два
 * констурктора с разной семантикой:
 * 1. Some(x) -- вычисление завершилось успешно, x -- его результат
 * 2. None -- вычисление завершилось неудачно, результата нет, увы
 */
fn try_extend_field<T>(
    f: &mut Field,
    solved_cb: impl Fn(&mut Field) -> T,
    next_step_cb: impl Fn(&mut Field) -> Option<T>,
) -> Option<T> {
    // Проверяем простые случаи:
    // 1. Поле противоречиво — решения нет.
    if f.contradictory() {
        return None;
    }
    // 2. Поле непротиворечиво, все заполнено => мы нашли решение.
    //    Надо вызвать наш колбэк-второй параметр и сообщить об успешной находке.
    if f.full() {
        return Some(solved_cb(f));
    }
    // Ищем первую непустую клетку.
    for row in 0..N {
        for col in 0..N {
            // Нашли пустую -- начинем перебирать все, что можно туда поставить
            if f.0[row][col] == Empty {
                for d in 1..=N {
                    f.0[row][col] = Digit(d);
                    // поставили -- вызвали колбэк(третий параметр)
                    // Здесь мы смотрим, если он вернул Some(x) -- значит мы нашли решение
                    // Надо его вернуть и дело с концом
                    // Иначе -- перебираем дальше
                    if let Some(x) = next_step_cb(f) {
                        return Some(x);
                    }
                    // И возвращаем все как было
                    f.0[row][col] = Empty;
                }
                return None;
            }
        }
    }
    // Эта строчка никогда не должна вызываться: поле непустое, а при нахождении
    // пустой клетке мы завершаем функцию. Если строчка-таки вызвалась, то это
    // ошибка программиста и программа "паникует" (валится с критической ошибкой).
    panic!("Field should've been non-full");
}

/// Перебирает все возможные решения головоломки, заданной параметром `f`.
/// Если хотя бы одно решение `s` существует, `f` оказывается равным `s`,
/// а функция возвращает `Some(f)`.
/// Если решений нет, `f` остаётся неизменным, а функция возвращает `None`.
fn find_solution(f: &mut Field) -> Option<Field> {
    try_extend_field(f, |f_solved| f_solved.clone(), find_solution)
}

/// Точка входа в нашу программу.
fn main() {
    // По подсказке компилятора: для корректного чтения строк через итератор
    // требуется, чтобы `BufRead` был "виден" в текущей функции.
    use std::io::BufRead;

    let stdin = std::io::stdin();
    // Читаем поле из stdin, заводим под него мутабельную переменную.
    // Так как `stdin.lock().lines()` возвращает итератор по `Result<String>`
    // (попыткам прочитать строчку), а не `String`, нам требуется "достать"
    // из каждой попытки реальную строчку. Это делает метод `.unwrap()`,
    // который при неудачной попытке чтения вызывает "панику" (критическую ошибку).
    let mut field = parse_field(stdin.lock().lines().map(|l| l.unwrap()));
    // stdin перестал быть нужен, избавимся от соответствующей переменной,
    // чтобы случайно не заиспользовать её потом.
    std::mem::drop(stdin);

    // Отладочный вывод поля, чтобы проверить корректность чтения.
    println!("{:?}", field);

    // Запускаем поиск решения.
    // Если оно есть, то печатаем его, в противном случае "паникуем"
    // (вызываем критическую ошибку) с сообщением `No solution`.
    // За "панику" отвечает вызов .expect() на типе `Option<>`.
    let solution = find_solution(&mut field).expect("No solution");
    println!("{:?}", solution);
}
