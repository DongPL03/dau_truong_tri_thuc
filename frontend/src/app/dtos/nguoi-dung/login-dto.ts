import {IsNotEmpty, IsString} from 'class-validator';

export class LoginDTO {
  @IsString()
  ten_dang_nhap?: string;

  @IsString()
  email?: string;

  @IsString()
  @IsNotEmpty()
  password: string;

  role_id: number;

  constructor(data: Partial<LoginDTO>) {
    this.ten_dang_nhap = data.ten_dang_nhap ?? '';
    this.email = data.email ?? '';
    this.password = data.password ?? '';
    this.role_id = data.role_id ?? 1;
  }
}
