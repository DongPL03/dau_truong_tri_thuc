import {IsEmail, IsNotEmpty, IsString} from 'class-validator';

export class RegisterDto {

  @IsString()
  ten_dang_nhap?: string = "";

  @IsEmail()
  email?: string = "";

  @IsString()
  ho_ten?: string = "";

  @IsString()
  @IsNotEmpty()
  password: string = "";

  retype_password: string = "";
  role_id: number | null = 1;

  constructor(data?: Partial<RegisterDto>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}
