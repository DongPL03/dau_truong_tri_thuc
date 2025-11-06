import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { environment } from '../../../environments/environment';

@Component({
  selector: 'app-address-selector',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './address-selector.html',
  styleUrl: './address-selector.scss'
})
export class AddressSelector implements OnInit {
  // 👉 Gọi thông qua backend
  private readonly api = `${environment.apiBaseUrl}/provinces`;

  provinces: any[] = [];
  wards: any[] = [];
  filteredWards: any[] = [];

  selectedProvince = '';
  selectedWard = '';

  @Output() addressChange = new EventEmitter<string>();

  constructor(private http: HttpClient) {}

  ngOnInit(): void {
    this.loadProvinces();
    this.loadWards();
  }

  loadProvinces(): void {
    this.http.get<any[]>(`${this.api}/p`).subscribe({
      next: data => (this.provinces = data),
      error: err => console.error('Lỗi tải provinces:', err)
    });
  }

  loadWards(): void {
    this.http.get<any[]>(`${this.api}/w`).subscribe({
      next: data => (this.wards = data),
      error: err => console.error('Lỗi tải wards:', err)
    });
  }

  onProvinceChange(): void {
    this.filteredWards = this.wards.filter(w => w.province_code == this.selectedProvince);
    this.selectedWard = '';
    this.emitAddress();
  }

  emitAddress(): void {
    const provinceName = this.provinces.find(p => p.code == this.selectedProvince)?.name || '';
    const wardName = this.filteredWards.find(w => w.code == this.selectedWard)?.name || '';
    const result = [wardName, provinceName].filter(Boolean).join(', ');
    this.addressChange.emit(result);
  }
}
