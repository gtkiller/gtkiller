
var application = {
    _fields: [
        {
            name: 'id',
            type: 'integer'
        },
        {
            name: 'name',
            type: 'text'
        },
        {
            name: 'password',
            type: 'text'
        },
        {
            name: 'email',
            type: 'text',
            events: [
                {
                    name: 'keyup',
                    handle: function (event)
                    {
                        this.form.elements.namedItem('jid').value =
                                this.value.replace('@', '-at-');
                    }
                }
            ]
        },
        {
            name: 'jid',
            type: 'text',
            readonly: true
        },
        {
            name: 'fingerprint',
            type: 'text',
            readonly: true
        },
        {
            name: 'active',
            type: 'checkbox'
        },
        {
            name: 'status',
            type: 'text'
        }
    ],
    _items: [],
    _controls: [
        {
            name:     'form',
            selector: 'div.form-box:first'
        },
        {
            name: 'list',
            selector: 'div.list-box:first'
        },
        {
            name: 'create-button',
            selector: 'input.create-button',
            events: [
                {
                    name: 'click',
                    handle: function (event)
                    {
                        application.showForm();
                    }
                }
            ]
        },
        {
            name: 'update-button',
            selector: 'input.update-button',
            events: [
                {
                    name: 'click',
                    handle: function (event)
                    {
                        application.showForm(true);
                    }
                }
            ]
        },
        {
            name: 'delete-button',
            selector: 'input.delete-button',
            events: [
                {
                    name: 'click',
                    handle: function (event)
                    {
                        application.execute('delete');
                    }
                }
            ]
        },
        {
            name: 'submit-button',
            selector: 'input.submit-button',
            events: [
                {
                    name: 'click',
                    handle: function (event)
                    {
                        application.submitForm();
                    }
                }
            ]
        },
        {
            name: 'cancel-button',
            selector: 'input.cancel-button',
            events: [
                {
                    name: 'click',
                    handle: function (event)
                    {
                        application.hideForm();
                    }
                }
            ]
        }
    ],
    _commands: [
        {
            name: 'select',
            check: function ()
            {
                return true;
            },
            make: function ()
            {
                return {
                    type: 'select'
                };
            },
            handle: function (result)
            {
                this._items = (result.data || []);
                this._refreshList();
            }
        },
        {
            name: 'create',
            check: function ()
            {
                return true;
            },
            make: function ()
            {
                return {
                    type: 'create',
                    data: this._serializeForm()
                };
            },
            handle: function (result)
            {
                var errors = (result.errors || []);

                if (errors.length)
                {
                    this._showFormErrors(errors);
                }
                else
                {
                    this._items.push(result.data);
                    this._refreshList();
                    this.hideForm();
                }
            }
        },
        {
            name: 'update',
            check: function ()
            {
                return true;
            },
            make: function ()
            {
                var item = this._serializeForm();
                var id   = item.id;

                delete item.id;

                return {
                    type: 'update',
                    id: id,
                    data: item
                };
            },
            handle: function (result)
            {
                var errors = (result.errors || []);

                if (errors.length)
                {
                    this._showFormErrors(errors);
                }
                else
                {
                    for (var index in this._items)
                    {
                        if (this._items[index].id == result.id)
                        {
                            this._items[index] = result.data;
                            this._items[index].id = result.id;
                            break;
                        }
                    }
                    this._refreshList();
                    this.hideForm();
                }
            }
        },
        {
            name: 'delete',
            check: function ()
            {
                return this._getSelectedItem() != null;
            },
            make: function ()
            {
                this.hideForm();

                return {
                    type: 'delete',
                    id: Number(this._getSelectedItem().id)
                };
            },
            handle: function (result)
            {
                this._removeItem(result.id);
                this._refreshList();
            }
        }
    ],

    _getArrayItem: function (array, field, value)
    {
        return $.grep(
            array,
            function (item) { return item[field] == value; }
        )[0] || null;
    },
    _getCommand: function (name)
    {
        return this._getArrayItem(this._commands, 'name', name);
    },
    _getControl: function (name)
    {
        return this._getArrayItem(this._controls, 'name', name);
    },
    _getItem: function (id)
    {
        return this._getArrayItem(this._items, 'id', id);
    },
    _getSelectedItem: function ()
    {
        var id = this._getControl('list')
                .element
                .find('input[name="item"]:checked:first')
                .val();

        return this._getItem(id);
    },
    _getItemIndex: function (id)
    {
        for (var index in this._items)
        {
            if (this._items[index].id == id)
            {
                return index;
            }
        }
        return -1;
    },
    _removeItem: function (id)
    {
        var index = this._getItemIndex(id);

        if (index != -1)
        {
            this._items.splice(index, 1);
        }
    },
    _replaceItem: function (item)
    {
        this._items[this._getItemIndex(item.id)] = item;
    },
    _fillForm: function (form, item)
    {
        this._fields.forEach(
            (function (form, item)
            {
                return function (value)
                {
                    var field = form.find('input[name="' + value.name + '"]:first');

                    switch (value.type)
                    {
                    case 'checkbox':
                        field.get(0).checked = Boolean(item[value.name]);
                        break;
                    default:
                        field.val(item[value.name] || '');
                        break;
                    }
                };
            }) (form, item)
        );
    },
    _serializeForm: function ()
    {
        var control = this._getControl('form');
        var form    = control.element.find('form:first');
        var item    = {};

        this._fields.forEach(
            (function (form, item)
            {
                return function (value)
                {
                    var field = form.find('input[name="' + value.name + '"]:first');

                    switch (value.type)
                    {
                    case 'checkbox':
                        item[value.name] = field.attr('checked') || false;
                        break;
                    case 'integer':
                        item[value.name] = Number(field.val()) || '';
                        break;
                    default:
                        item[value.name] = field.val() || '';
                        break;
                    }
                };
            }) (form, item)
        );

        return item;
    },
    _showFormErrors: function (errors)
    {
        var control = this._getControl('form');
        var form    = control.element.find('form:first');

        this._fields.forEach(
            (function (form, errors)
            {
                return function (value)
                {
                    var field = form.find('input[name="' + value.name + '"]:first');
                    var error = errors
                            .filter(function (value) { return value.field == this.name; }, value)
                            .map(function (value) { return value.description })
                            .join('<br />');

                    if (error.length)
                    {
                        field.parent().append('<span class="error-text">' + error + '</span>');
                    }
                    else
                    {
                        field.parent().find('.error-text').remove();
                    }
                };
            }) (form, errors)
        );
    },
    _appendToList: function (item)
    {
        var control = this._getControl('list');
        var list    = control.element.find('table:first').get(0).tBodies[0];
        var html =
                '<tr>' +
                    '<td><input type="radio" name="item" value="' + item.id + '" /></td>' +
                    '<td>' + item.id + '</td>' +
                    '<td>' + item.name + '</td>' +
                    '<td>' + item.email + '</td>' +
                    '<td>' + item.active + '</td>' +
                '</tr>';

        $(list).append(html);
    },
    _refreshList: function ()
    {
        var control = this._getControl('list');
        var list    = control.element.find('table:first').get(0).tBodies[0];

        while (list.rows.length)
        {
            list.deleteRow(0);
        }
        this._items.forEach(this._appendToList, this);
    },
    _makeRequest: function ()
    {
        return {
            commands: Array.prototype.slice.call(arguments)
        };
    },
    _handleRequest: function (data)
    {
        var response = $.parseJSON(data);

        (response.commands || []).map(
            function (result)
            {
                this._getCommand(result.type).handle.call(this, result);
            },
            this
        );
    },

    initialize: function ()
    {
        for (var control in this._controls)
        {
            control = this._controls[control];
            control.element = $(control.selector);
            for (var event in control.events)
            {
                event = control.events[event];
                control.element.bind(event.name, event.handle);
            }
        }

        var form = this._getControl('form').element;

        for (var field in this._fields)
        {
            field = this._fields[field];
            field.element = form.find('[name="' + field.name + '"]');
            for (var event in field.events)
            {
                event = field.events[event];
                field.element.bind(event.name, event.handle);
            }
        }

        this.execute('select');
    },
    execute: function (name)
    {
        var command = this._getCommand(name);

        if (command && command.check.call(this))
        {
            $.ajax({
                url: '/admin/json/account',
                type: 'POST',
                data: 'request=' + $.toJSON(this._makeRequest(command.make.call(this))),
                context: this,
                success: this._handleRequest
            });
        }
    },
    showForm: function (mode)
    {
        var control = this._getControl('form');
        var form    = control.element.find('form:first');

        if (mode)
        {
            var item = this._getSelectedItem();

            if (item)
            {
                control.element.show();
                this._fillForm(form, item);
                form.get(0).elements[0].focus();
            }
        }
        else
        {
            control.element.show();
            form.get(0).elements[0].focus();
        }
    },
    hideForm: function ()
    {
        var control = this._getControl('form');
        var form    = control.element.find('form:first');

        control.element.hide();
        this._fillForm(form, {});
        this._showFormErrors([]);
    },
    submitForm: function ()
    {
        var control = this._getControl('form');
        var form    = control.element.find('form:first');

        if (form.get(0).elements.namedItem('id').value)
        {
            this.execute('update');
        }
        else
        {
            this.execute('create');
        }
    }
};

$(document).ready(
    function ()
    {
        application.initialize();
    }
);
