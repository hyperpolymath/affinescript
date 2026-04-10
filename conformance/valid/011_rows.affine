// Conformance test: row polymorphism

fn get_x[..r](point: {x: Int, ..r}) -> Int {
  point.x
}

fn get_name[..r](entity: {name: String, ..r}) -> String {
  entity.name
}

fn with_id[..r](record: {..r}, id: Int) -> {id: Int, ..r} {
  { id: id, ..record }
}

type HasPosition[..r] = {x: Int, y: Int, ..r}
type HasName[..r] = {name: String, ..r}
