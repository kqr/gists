
class Lens:
    def __init__(self, attr=None, getter=None, setter=None):
        # If initialised with an attr, automatically create getters and
        # setters. If initialised with getter and setter, create a lens
        # from those (slightly more manual, but also more flexible!)
        if attr and not getter and not setter:
            def getter(obj):
                return getattr(obj, attr)
            def setter(obj, value):
                setattr(obj, attr, value)
                return obj

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

    name, eyes, colour = Lens('name'), Lens('eyes'), Lens('colour')
    car, tires, wear = Lens('car'), Lens('tires'), Lens('wear')
    # The emissions are read-only, because you can not cheat your way
    # out of greenhouse gas taxes
    emissions = Lens(getter=lambda obj: getattr(obj, 'emissions'))


    #######################
    ### Create a person

    arne = Person('Arne', Eyes('Green'), Car(9001, Tires(0.4)))

    print('{} has {} eyes.'.format(
        view(name)(arne),
        # Lenses can be composed with |
        view(eyes | colour)(arne)
    ))

    # view(Lens(attr)) = getattr(attr)
    impact = view(car | emissions)
    print('They have an environmental impact that is {}.'.format(
        impact(arne) if impact(arne) <= 9000 else 'OVER NINE THOUSAND'
    ))

    # You can make long chains of lenses with |
    print('Their car has {:.0f} % worn tires.'.format(
        view(car | tires | wear)(arne)*100    
    ))



    #######################
    ### Perform some updates

    # set(Lens(attr)) = setattr(attr)
    set_eye_colour = set(eyes | colour)
    set_eye_colour('Blue')(arne)
    # Lenses can be aliased
    tirewear = tires | wear
    modify(car | tirewear)(lambda i: i*1.1)(arne)

    print('{} now has {} eyes'.format(
        view(name)(arne),
        view(eyes | colour)(arne)
    ))
    print('Their car now has {:.0f} % worn tires.'.format(
        view(car | tires | wear)(arne)*100    
    ))


    try:
        # Trying to cheat the taxes...
        set(car | emissions)(300)(arne)
    except ValueError as e:
        # Will blow up. Since `emissions` is a read-only lens, the composition
        # `car | emissions` will also be read only.
        print(e)



    #######################
    ### Random example of lenses for immutable values (tuples are immutable)

    first = Lens(getter=lambda t: t[0], setter=lambda t, v: (v, *t[1:]))
    t = (3, 8, 5)
    print('Tuple is {}'.format(t))
    new_t = set(first)(42)(t)
    print('New tuple is {}'.format(new_t))

