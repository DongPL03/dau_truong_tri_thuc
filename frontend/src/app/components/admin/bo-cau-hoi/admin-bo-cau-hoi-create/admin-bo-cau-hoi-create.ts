import {Component, OnInit, ViewChild} from '@angular/core';
import {Base} from '../../../base/base';
import {FormsModule, NgForm} from '@angular/forms';
import {ChuDe} from '../../../../models/chude';
import {ResponseObject} from '../../../../responses/response-object';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-admin-bo-cau-hoi-create',
  imports: [
    FormsModule
  ],
  standalone: true,
  templateUrl: './admin-bo-cau-hoi-create.html',
  styleUrl: './admin-bo-cau-hoi-create.scss',
})
export class AdminBoCauHoiCreate extends Base implements OnInit {
  @ViewChild('createForm') createForm!: NgForm;

  loading = false;

  chu_de_list: ChuDe[] = [];

  form: any = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PRIVATE' // gợi ý: admin tạo bộ thi đấu → để PRIVATE
  };

  ngOnInit(): void {
    this.loadChuDe();
  }

  loadChuDe(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chu_de_list = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  submit(): void {
    if (!this.createForm || this.createForm.invalid) {
      // Có thể báo lỗi nhẹ cho user
      Swal.fire('Thiếu dữ liệu', 'Vui lòng kiểm tra lại các trường bắt buộc', 'warning').then(r => {});
      return;
    }

    this.loading = true;

    this.bocauHoiService.create(this.form).subscribe({
      next: (res: ResponseObject<any>) => {
        this.loading = false;
        const created = res.data;
        Swal.fire('Thành công', 'Đã tạo bộ câu hỏi mới', 'success').then(() => {
          if (created?.id) {
            this.router.navigate(['/admin/bo-cau-hoi', created.id]).then(r => {});
          } else {
            this.router.navigate(['/admin/bo-cau-hoi']).then(r => {});
          }
        });
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tạo bộ câu hỏi', 'error').then(r => {});
      }
    });
  }

  cancel(): void {
    this.router.navigate(['/admin/bo-cau-hoi']);
  }

}
