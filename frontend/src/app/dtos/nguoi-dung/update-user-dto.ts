export class UpdateUserDTO {
  ho_ten?: string;
  ten_hien_thi?: string;
  email?: string;
  dia_chi?: string;

  constructor(data?: Partial<UpdateUserDTO>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}
