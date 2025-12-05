import {UserResponse} from './user-response';

export class UserListResponse {
  users: UserResponse[];
  total_pages: number;

  constructor(users: UserResponse[], total_pages: number) {
    this.users = users;
    this.total_pages = total_pages;
  }
}
