import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ChuDe} from '../../../models/chude';

@Component({
  selector: 'app-bo-cau-hoi-create',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './create.html',
  styleUrl: './create.scss'
})
export class BoCauHoiCreate extends Base implements OnInit {
  @ViewChild('createForm') createForm!: NgForm;

  // Biến dữ liệu
  chuDes: ChuDe[] = [];
  creating = false;

  // Dữ liệu bind form
  model = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PUBLIC'
  };


  ngOnInit(): void {
    this.loadChuDe();
  }

  /** Gọi API lấy danh sách chủ đề */
  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  /** Submit form tạo bộ câu hỏi */
  onSubmit(form: NgForm) {
    if (form.invalid || this.model.chu_de_id === 0) {
      Swal.fire('Cảnh báo', 'Vui lòng điền đủ thông tin bắt buộc', 'warning').then(r => {
      });
      return;
    }

    this.creating = true;

    this.bocauHoiService.create(this.model).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => {
        Swal.fire('Thành công', 'Tạo bộ câu hỏi thành công!', 'success').then(() => {
          this.router.navigateByUrl('/bo-cau-hoi/list').then(r => {
          });
        });
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể tạo bộ câu hỏi', 'error').then(r => {
        });
      },
      complete: () => (this.creating = false)
    });
  }

//   next: (res: ResponseObject<BoCauHoiResponse>) => {
//     const created = res.data;
//
//     if (created?.trang_thai === 'DA_DUYET') {
//   Swal.fire({
//               icon: 'success',
//               title: 'Tạo thành công!',
//               text: 'Bạn có thể thêm câu hỏi ngay bây giờ.',
//               confirmButtonText: 'Thêm câu hỏi'
//             }).then(() => {
//   this.router.navigateByUrl(`/questions/create/${created.id}`);
// });
// } else {
// Swal.fire({
// icon: 'info',
// title: 'Đang chờ duyệt',
// text: 'Bộ câu hỏi của bạn cần được Admin phê duyệt trước khi thêm câu hỏi.',
// confirmButtonText: 'Về danh sách'
// }).then(() => {
// this.router.navigateByUrl('/quizzes');
// });
// }
// },
// error: (err) => {
// Swal.fire('Lỗi', err.error?.message || 'Không thể tạo bộ câu hỏi', 'error');
// },
// complete: () => (this.creating = false)
// });
// }

  cancel() {
    this.router.navigateByUrl('/bo-cau-hoi/list').then(r => {
    });
  }
}
