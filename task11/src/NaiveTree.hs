{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge Nil right = right
merge (Node keyL valueL leftL rightL) right = Node keyL valueL leftL (merge rightL right)


{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton key value = Node key value Nil Nil

    toAscList Nil = []
    toAscList (Node key value left right) = toAscList left ++ [(key, value)] ++ toAscList right

    alter f key Nil = maybe empty (singleton key) $ f Nothing
    alter f key (Node rkey rval rleft rright)
        | key < rkey = merge (alter f key rleft) (Node rkey rval Nil rright)
        | key > rkey = merge (Node rkey rval rleft Nil) (alter f key rright)
        | otherwise  = case f (Just rval) of
            Just x -> merge (merge rleft (Node key x Nil Nil)) rright
            Nothing -> merge rleft rright

    lookup _ Nil = Nothing
    lookup key (Node rkey rval rleft rright)
        | key < rkey = Map.lookup key rleft
        | key > rkey = Map.lookup key rright
        | otherwise  = Just rval

    null Nil = True
    null _ = False

    size Nil = 0
    size (Node _ _ left right) = 1 + size left + size right
