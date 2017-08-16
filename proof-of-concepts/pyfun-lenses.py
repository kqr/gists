
class Lens:
    def __init__(self, getter=None, setter=None):
        'Low-level method to create a lens from a getter and a setter'
        self.getter = getter
        self.setter = setter

    def __or__(self, other):
        'Compose two lenses into a new lens'

        def new_getter(obj):
            # Chain the two getters into a single getter
            return other.getter(self.getter(obj))
        def new_setter(obj, value):
            # Chain the two setters into a single setter
            inner_obj = self.getter(obj)
            other.setter(inner_obj, value)
            self.setter(obj, inner_obj)
            # Setters return objects because that allows us to use lenses
            # with immutable values!
            return obj

        # If the setter is None, the lens is "read-only". If any of the two
        # setters are None, then also the composition of the two lenses is
        # "read only". Same thing applies for getters. A lens can be "write
        # only".
        return Lens(
            getter=self.getter and other.getter and new_getter,
            setter=self.setter and other.setter and new_setter,
        )


def make_lens(attr):
    'Make a lens from the attr of a mutable object'

    def getter(obj):
        return getattr(obj, attr)

    def setter(obj, value):
        setattr(obj, attr, value)
        return obj

    return Lens(getter=getter, setter=setter)



def view(lens):
    'Creates a getter from a lens'
    if lens.getter:
        return lens.getter
    else:
        raise ValueError('Lens is not readable!')


def set(lens):
    'Creates a setter from a lens'
    if lens.setter:
        return lambda value: lambda obj: lens.setter(obj, value)
    else:
        raise ValueError('Lens is not writeable!')


def modify(lens):
    'Creates a modifier from a lens'
    # This method is buggy with the current implementation. A correct
    # implementation should allow modifying as long as there is a setter,
    # even if there is no getter!
    if lens.getter and lens.setter:
        return lambda function: lambda obj: lens.setter(obj, function(lens.getter(obj)))
    else:
        raise ValueError('Lens is not read/writeable!')


if __name__ == '__main__':
    class Person:
        'A person has a name, a car and eyes'
        def __init__(self, name, eyes, car):
            self.name = name
            self.eyes = eyes
            self.car = car
    class Eyes:
        'Eyes have a colour'
        def __init__(self, colour):
            self.colour = colour
    class Car:
        'Cars have an environmental CO2 impact and tires'
        def __init__(self, emissions, tires):
            self.emissions = emissions
            self.tires = tires
    class Tires:
        'Tires have some wear'
        def __init__(self, wear):
            self.wear = wear

    name, eyes, colour = make_lens('name'), make_lens('eyes'), make_lens('colour')
    car, tires, wear = make_lens('car'), make_lens('tires'), make_lens('wear')
    # The emissions are read-only, because you can not cheat your way
    # out of greenhouse gas taxes
    emissions = Lens(getter=lambda obj: getattr(obj, 'emissions'))


    #######################
    ### Create a person

    arne = Person('Arne', Eyes('Green'), Car(9001, Tires(0.4)))

    print('{} has {} eyes.'.format(
        view (name) (arne),
        # Lenses can be composed with |
        view (eyes|colour) (arne)
    ))

    # view(Lens(attr)) = getattr(attr)
    impact = view (car|emissions)
    print('They have an environmental impact that is {}.'.format(
        impact(arne) if impact(arne) <= 9000 else 'OVER NINE THOUSAND'
    ))

    # You can make arbitrarily long chains of lenses with |
    print('Their car has {:.0f} % worn tires.'.format(
        100 * view (car|tires|wear) (arne)
    ))



    #######################
    ### Perform some updates

    # set(Lens(attr)) = setattr(attr)
    set_eye_colour = set (eyes|colour)
    set_eye_colour ('Blue') (arne)
    
    # Lenses can be aliased
    tirewear = tires|wear
    # Now you can replace "tires|wear" in all your lens
    # chains with the new "tirewear" lens
    modify (car|tirewear) (lambda i: i*1.1) (arne)

    print('{} now has {} eyes'.format(
        view (name) (arne),
        view (eyes|colour) (arne)
    ))
    print('Their car now has {:.0f} % worn tires.'.format(
        100 * view (car|tires|wear) (arne)   
    ))


    try:
        # Trying to cheat the taxes...
        set (car|emissions) (300) (arne)
    except ValueError as e:
        # Will blow up. Since `emissions` is a read-only lens, the composition
        # `car | emissions` will also be read only.
        print(e)



    #######################
    ### Random example of lenses for immutable values

    # Make sure the setter returns a new value instead of trying to modify
    # the original value!
    first = Lens(getter=lambda t: t[0], setter=lambda t, v: (v, *t[1:]))

    # Tuples are immutable
    triple = (3, 8, 5)
    print('Triple is {}'.format(triple))

    # Setter returns a new tuple instead of mutating the old one
    new_triple = set (first) (42) (triple)
    print('New triple is {}'.format(new_triple))

